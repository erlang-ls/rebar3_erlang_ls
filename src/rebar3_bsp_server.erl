-module(rebar3_bsp_server).
-behaviour(gen_server).

-export([ start_link/1 ]).

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

-type state() :: #{ rebar3_state := rebar_state:t()     %% The rebar3 state
                  , is_initialized := boolean()         %% Build server initialized?
                  , is_shutdown := boolean()            %% Build server has shut down?
                  , port := port() | pid() | undefined  %% IO Port
                  , buffer := binary()                  %% Data buffer
                  , messages := list()                  %% Pending incoming messages
                  }.

-export_type([state/0]).

-spec start_link(map()) -> {ok, pid()}.
start_link(InitialState) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, InitialState, []).

-spec init(map()) -> {ok, state()}.
init(InitialState) ->
  State = lists:foldl(fun(Key, StateAcc) ->
                          case maps:is_key(Key, StateAcc) of
                            true ->
                              StateAcc;
                            false ->
                              StateAcc#{ Key => default_value(Key) }
                          end
                      end,
                      InitialState,
                      [ rebar3_state
                      , is_initialized
                      , is_shutdown
                      , port
                      , buffer
                      , messages
                      ]),
  {ok, State}.

default_value(is_initialized) ->
  false;
default_value(is_shutdown) ->
  false;
default_value(port) ->
  IoFdString = os:getenv(?IO_FDS_ENV_VARIABLE, ?DEFAULT_IO_FDS),
  {ok, [InFd, OutFd], _Garbage} = io_lib:fread("~d ~d", IoFdString),
  erlang:open_port({fd, InFd, OutFd}, [binary]);
default_value(buffer) ->
  <<>>;
default_value(messages) ->
  [].

handle_call(_Request, _From, _State) ->
  error(badarg).

handle_cast(_Request, _State) ->
  error(badarg).

handle_info({Port, {data, NewData}}, #{ port := Port, buffer := OldBuffer } = State) ->
  {noreply, State#{ buffer => <<OldBuffer/binary, NewData/binary>> }, {continue, decode}}.

handle_continue(decode, #{ buffer := Buffer, messages := OldMsgs } = State) ->
  case rebar3_bsp_protocol:decode_packets(Buffer) of
    {ok, NewMsgs, RestData} ->
      {noreply, State#{ buffer => RestData, messages => OldMsgs ++ NewMsgs }, {continue, messages}};
    {error, Reason} ->
      {stop, State, {decode_error, Reason}}
  end;
handle_continue(messages, #{ messages := [] } = State) ->
  {noreply, State};
handle_continue(messages, State) ->
  {noreply, handle_message(State), {continue, messages}}.

handle_message(#{ messages := [M|Ms] } = State) ->
  MessageType = rebar3_bsp_protocol:message_type(M),
  case dispatch_message(MessageType, M, State#{ messages => Ms }) of
    {response, Reply, NewState} ->
      ok = send_message(Reply, NewState),
      NewState;
    {noresponse, NewState} ->
      NewState
  end.

send_message(Message, #{ port := Port } = _State) ->
  ok = rebar3_bsp_protocol:send_message(Port, Message),
  ok.

dispatch_message(request, #{ id := Id } = Message, State) ->
  case try_dispatch(Message, State) of
    {response, Result, NewState} ->
      {response, rebar3_bsp_protocol:response(Id, Result), NewState};
    {error, Error, NewState} ->
      {response, rebar3_bsp_protocol:error(Id, Error), NewState};
    {noresponse, NewState} ->
      {response, rebar3_bsp_protocol:response(Id, null), NewState}
  end;
dispatch_message(response, Message, State) ->
  ?LOG_WARNING("Ignoring response ~p", [Message]),
  {noresponse, State};
dispatch_message(notification, Message, State) ->
  case try_dispatch(Message, State) of
    {response, Result, NewState} ->
      ?LOG_WARNING("Unexpected response to notification [notification=~p] [response=~p]",
                   [Message, Result]),
      {noresponse, NewState};
    {error, Error, NewState} ->
      {response, rebar3_bsp_protocol:error(null, Error), NewState};
    {noresponse, NewState} ->
      {noresponse, NewState}
  end.

try_dispatch(#{ method := Method } = Message, State) ->
  Params = maps:get(params, Message, #{}),
  case erlang:function_exported(rebar3_bsp_methods, Method, 2) of
    false ->
      %% XXX hard coded stuff is evil, -32601 means MethodNotFound
      {error, #{ code => -32601, message => <<"Unsupported method ", Params/binary>> }, State};
    true ->
      rebar3_bsp_methods:Method(Params, State)
  end.

