-module(rebar3_bsp_server).
-behaviour(gen_server).

-export([ start_link/1
        , stop/0
        , post_message/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , format_status/2
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
                  }.

-export_type([state/0]).

-spec start_link(map()) -> {ok, pid()}.
start_link(InitialState) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, InitialState, []).

-spec stop() -> ok.
stop() ->
  gen_server:call(?SERVER, {shutdown, normal}).

-spec post_message(map()) -> ok.
post_message(Message) ->
  gen_server:cast(?SERVER, {incoming_message, Message}).

-spec init(map()) -> {ok, state()}.
init(InitialState) ->
  State = lists:foldl(fun(Key, StateAcc) ->
                          case maps:is_key(Key, StateAcc) of
                            true ->
                              StateAcc;
                            false ->
                              Value = default_value(Key),
                              StateAcc#{ Key => Value }
                          end
                      end,
                      InitialState,
                      [ rebar3_state
                      , is_initialized
                      , is_shutdown
                      , port
                      , buffer
                      ]),
  {ok, State}.

-spec default_value(atom()) -> term().
default_value(is_initialized) ->
  false;
default_value(is_shutdown) ->
  false;
default_value(port) ->
  IoFdString = os:getenv(?IO_FDS_ENV_VARIABLE, ?DEFAULT_IO_FDS),
  {ok, [InFd, OutFd], _Garbage} = io_lib:fread("~d ~d", IoFdString),
  erlang:open_port({fd, InFd, OutFd}, [binary]);
default_value(buffer) ->
  <<>>.

-spec handle_call({shutdown, term()}, term(), state()) -> {stop, term(), ok, state()}.
handle_call({shutdown, Reason}, _From, State) ->
  ReportedReason = case Reason of
                     normal ->
                       normal;
                     {exit_code, 0} ->
                       normal;
                     _ ->
                       {shutdown, Reason}
                   end,
  {stop, ReportedReason, ok, State}.

-spec handle_cast({incoming_message, map()}, state()) -> {noreply, state()}.
handle_cast({incoming_message, Message}, State) ->
  MessageType = rebar3_bsp_protocol:message_type(Message),
  case dispatch_message(MessageType, Message, State) of
    {response, Reply, #{ port := Port } = NewState} ->
      ok = rebar3_bsp_protocol:send_message(Port, Reply),
      {noreply, NewState};
    {noresponse, NewState} ->
      {noreply, NewState};
    {exit, ExitCode, NewState} ->
      RequestId = gen_server:send_request(self(), {shutdown, {exit_code, ExitCode}}),
      gen_server:receive_response(RequestId, 0),
      {noreply, NewState}
  end.

-spec handle_info({pid() | port(), {data, binary()}}, state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({Port, {data, NewData}}, #{ port := Port, buffer := OldBuffer } = State) ->
  RestData = rebar3_bsp_protocol:peel_messages(fun post_message/1, <<OldBuffer/binary, NewData/binary>>),
  {noreply, State#{ buffer => RestData }}.

-spec format_status(normal | terminate, list()) -> term().
format_status(_Opt, [_PDict, State]) ->
  [{data, [{"State", State#{ rebar3_state => rebar3_state_redacted }}]}].

-spec dispatch_message(atom(), map(), state()) ->
        {noresponse, state()} |
        {response, map(), state()} |
        {exit, integer(), state()}.
dispatch_message(response, Message, State) ->
  ?LOG_WARNING("Ignoring response ~p", [Message]),
  {noresponse, State};
dispatch_message(MessageType, Message, State) ->
  case {MessageType, try_dispatch(Message, State)} of
    %% Requests must always be responded to
    {request, {response, Result, NewState}} ->
      {response, make_response(Message, Result), NewState};
    {request, {noresponse, NewState}} ->
      {response, make_response(Message, null), NewState};
    %% Notifications don't expect responses
    {notification, {response, Result, NewState}} ->
      ?LOG_WARNING("Unexpected response to notification [notification=~p] [response=~p]",
                   [Message, Result]),
      {noresponse, NewState};
    {notification, {noresponse, NewState}} ->
      {noresponse, NewState};
    %% Always report errors
    {_AnyType, {error, Error, NewState}} ->
      {response, make_error(Message, Error), NewState};
    %% Always forward exit requests
    {_AnyType, {exit, ExitCode, NewState}} ->
      {exit, ExitCode, NewState}
  end.

-spec make_response(map(), map() | null) -> map().
make_response(Message, Result) ->
  Id = rebar3_bsp_protocol:message_id(Message),
  rebar3_bsp_protocol:response(Id, Result).

-spec make_error(map(), map()) -> map().
make_error(Message, Error) ->
  Id = rebar3_bsp_protocol:message_id(Message),
  rebar3_bsp_protocol:error(Id, Error).

-spec try_dispatch(map(), state()) -> term().
try_dispatch(#{ method := Method } = Message, State) ->
  Params = maps:get(params, Message, #{}),
  MethodAtom = erlang:binary_to_atom(Method),
  try
    rebar3_bsp_methods:MethodAtom(Params, State)
  catch
    Class:Error:Stacktrace ->
      {Code, Msg} = case erlang:function_exported(rebar3_bsp_methods, MethodAtom, 2) of
                      false ->
                        {?LSP_ERROR_METHOD_NOT_FOUND, <<"Unsupported method ", Method/binary>>};
                      true ->
                        {?LSP_ERROR_INTERNAL_ERROR, io_lib:format("~p:~p ~p", [Class, Error, Stacktrace])}
                    end,
      {error, #{ code => Code, message => rebar3_bsp_util:to_binary(Msg) }, State}
  end.

