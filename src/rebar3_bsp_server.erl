-module(rebar3_bsp_server).
-behaviour(gen_server).

-export([ start_link/1
        , start_link/2
        , stop/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        ]).

-include("rebar3_bsp.hrl").

-define(SERVER, ?MODULE).
-define(IO_FDS_ENV_VARIABLE, "REBAR3_BSP_IO_FDS").
-define(DEFAULT_IO_FDS, "0 1").

-type server_port() :: port() | pid() | undefined.
-type state() :: #{ agent := pid() | undefined % Our agent
                  , port := server_port()      % IO Port
                  , buffer := binary()         % data buffer
                  , pending := map()           % Request responses that are pending
                  }.

-spec start_link(rebar_state:t()) -> {ok, pid()}.
start_link(R3State) ->
  start_link(R3State, undefined).

-spec start_link(rebar_state:t(), server_port()) -> {ok, pid()}.
start_link(R3State, Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {R3State, Port}, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec init({rebar_state:t(), server_port()}) -> {ok, state()}.
init({R3State, Port}) ->
  process_flag(trap_exit, true),
  {ok, Agent} = rebar3_bsp_agent:start_link(R3State),
  true = link(Agent),
  {ok, #{ agent => Agent
        , port => make_port(Port)
        , buffer => <<>>
        , pending => #{}
        }}.

-spec make_port(server_port()) -> pid() | port().
make_port(undefined) ->
  IoFdString = os:getenv(?IO_FDS_ENV_VARIABLE, ?DEFAULT_IO_FDS),
  {ok, [InFd, OutFd], _Garbage} = io_lib:fread("~d ~d", IoFdString),
  erlang:open_port({fd, InFd, OutFd}, [binary]);
make_port(Port) ->
  Port.

handle_call(Request, From, State) ->
  error(badarg, [Request, From, State]).

handle_cast(Request, State) ->
  error(badarg, [Request, State]).

handle_info({Port, {data, NewData}}, #{ port := Port, buffer := OldBuffer } = State) ->
  {noreply, State#{ buffer => <<OldBuffer/binary, NewData/binary>> }, {continue, decode}};
handle_info({'EXIT', Port, Reason}, #{ port := Port } = State) ->
  {stop, Reason, State#{ port => undefined }};
handle_info({'EXIT', Agent, Reason}, #{ agent := Agent } = State) ->
  {stop, Reason, State#{ agent => undefined }};
handle_info(Msg, State) ->
  case handle_pending(Msg, State) of
    {ok, NewState} ->
      {noreply, NewState};
    no_reply ->
      ?LOG_CRITICAL("Discarding message [message=~p]", [Msg]),
      {noreply, State}
  end.

-spec handle_continue(decode, state()) -> {noreply, state()} | {noreply, state(), {continue, decode}} | {stop, term(), state()}.
handle_continue(decode, #{ buffer := Buffer } = State) ->
  case rebar3_bsp_protocol:peel_message(Buffer) of
    {ok, Message, Rest} ->
      NewState = handle_message(Message, State),
      {noreply, NewState#{ buffer => Rest }, {continue, decode}};
    {more, _More} ->
      {noreply, State};
    {error, Reason, Buffer} ->
      %% We got back our input buffer, if we loop now we will do so forever. All we can do is bail.
      ?LOG_EMERGENCY("Decode error without progress, aborting. [error=~p]", [Reason]),
      {stop, {error, {bsp_protocol_error, Reason}}, State};
    {error, Reason, Rest} ->
      %% The buffer we got is not the original one - some progress happened. Try to recover.
      {Skipped, Rest} = erlang:split_binary(Buffer, erlang:byte_size(Buffer) - erlang:byte_size(Rest)),
      ?LOG_ALERT("Decode error. Trying to continue. [error=~p] [skipped=~p]", [Reason, Skipped]),
      {noreply, State#{ buffer => Rest }, {continue, decode}}
  end.

-spec handle_message(map(), state()) -> state().
handle_message(Message, State) ->
  case rebar3_bsp_protocol:message_type(Message) of
    response ->
      ok = rebar3_bsp_agent:post_response(Message),
      State;
    notification ->
      Method = maps:get(method, Message),
      Params = maps:get(params, Message, null),
      ok = rebar3_bsp_agent:notify(Method, Params),
      State;
    request ->
      Method = maps:get(method, Message),
      Params = maps:get(params, Message, null),
      Id = rebar3_bsp_agent:post_request(Method, Params),
      #{ pending := OldPending } = State,
      NewPending = OldPending#{ Id => Message },
      State#{ pending => NewPending }
  end.

-spec handle_pending(term(), state()) -> {ok, state()} | no_reply.
handle_pending(Msg, #{ port := Port, pending := Pending } = State) ->
  F = fun({Id, Request}) ->
          case rebar3_bsp_agent:check_response(Msg, Id) of
            no_reply ->
              true;
            {reply, Reply} ->
              Content = case Reply of
                          {ok, Response} ->
                            make_response(Request, Response);
                          {error, Error} ->
                            make_error(Request, Error)
                        end,
              rebar3_bsp_protocol:send_message(Port, Content),
              false;
            {error, Error} ->
              ?LOG_ALERT("Error checking replies. [error=~p]", [Error]),
              false
          end
      end,
  case lists:splitwith(F, maps:to_list(Pending)) of
    {_, []} ->
      %% Nothing matched
      no_reply;
    {Left, [_Match|Right]} ->
      {ok, State#{ pending => maps:from_list(Left ++ Right) }}
  end.

-spec make_response(map(), map() | null) -> map().
make_response(Message, Result) ->
  Id = rebar3_bsp_protocol:message_id(Message),
  rebar3_bsp_protocol:response(Id, Result).

-spec make_error(map(), map()) -> map().
make_error(Message, Error) ->
  Id = rebar3_bsp_protocol:message_id(Message),
  rebar3_bsp_protocol:error(Id, Error).

