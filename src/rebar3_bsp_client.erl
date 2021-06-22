%%==============================================================================
%% A client for the Build Server Protocol using the STDIO transport
%%==============================================================================
-module(rebar3_bsp_client).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================
%% Erlang API
-export([ start_link/1
        , stop/0
        , send_request/2
        , receive_response/2
        , check_response/2
        , wait_response/2
        , send_notification/2
        , get_requests/0
        , get_notifications/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        ]).
%%==============================================================================
%% Includes
%%==============================================================================
-include("rebar3_bsp.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { request_id    = 1    :: request_id()
               , pending       = []   :: [{request_id(), from()}]
               , notifications = []   :: [notificationMessage()]
               , requests      = []   :: [requestMessage()]
               , messages      = []   :: [requestMessage() | responseMessage() | notificationMessage()]
               , buffer        = <<>> :: binary()
               , port                 :: port() | pid() | undefined
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()      :: #state{}.
-type request_id() :: pos_integer().
-type from()       :: {pid(), any()}.
-type start_param() :: {root, string()}
                     | {exec, string(), [string()], [term()]}
                     | {port, port() | pid()}.

-type method() :: atom() | binary().

%%==============================================================================
%% Erlang API
%%==============================================================================
-spec start_link(start_param()) -> {ok, pid()}.
start_link({root, RootPath}) ->
  {ok, Executable, Args} = rebar3_bsp_connection:discover(RootPath),
  start_link({exec, Executable, Args, [{cd, RootPath}]});
start_link({exec, Executable, Args, PortSettings}) ->
  start_link_impl({ open_port
                  , {spawn_executable, Executable}
                  , PortSettings ++ [{args, Args}, use_stdio, binary, exit_status]
                  });
start_link({port, Port}) ->
  start_link_impl({reuse_port, Port}).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec send_request(method(), params()) -> any().
send_request(Method, Params) ->
  gen_server:send_request(?SERVER, {send_request, ensure_binary(Method), Params}).

-spec receive_response(any(), timeout()) -> {ok, responseResult()} | {error, responseError()} | timeout.
receive_response(RequestId, Timeout) ->
  case gen_server:receive_response(RequestId, Timeout) of
    {reply, Response} ->
      unpeel_response(Response);
    timeout ->
      timeout
  end.

-spec check_response(any(), any()) -> {ok, responseResult()} | {error, responseError()} | no_reply.
check_response(Msg, RequestId) ->
  case gen_server:check_response(Msg, RequestId) of
    {reply, Response} ->
      unpeel_response(Response);
    no_reply ->
      no_reply
  end.

-spec wait_response(any(), timeout()) -> {ok, responseResult()} | {error, responseError()} | timeout.
wait_response(RequestId, Timeout) ->
  case gen_server:wait_response(RequestId, Timeout) of
    {reply, Response} ->
      unpeel_response(Response);
    timeout ->
      timeout
  end.

-spec send_notification(method(), params()) -> ok.
send_notification(Method, Params) ->
  gen_server:cast(?SERVER, {send_notification, ensure_binary(Method), Params}).

-spec get_requests() -> [requestMessage()].
get_requests() ->
  gen_server:call(?SERVER, get_requests).

-spec get_notifications() -> [notificationMessage()].
get_notifications() ->
  gen_server:call(?SERVER, get_notification).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init({open_port | reuse_port, any()}) -> {ok, state()}.
init(PortSpec) ->
  ?LOG_DEBUG("Initializing with PortSpec = ~p", [PortSpec]),
  Port = case PortSpec of
           {open_port, PortName, PortSettings} ->
             erlang:open_port(PortName, PortSettings);
           {reuse_port, P} ->
             P
         end,
  {ok, #state{port = Port}}.

-define(CALL_SPEC(Request, Result), (Request, from(), state()) -> Result).
-spec handle_call?CALL_SPEC({send_request, binary(), params()}, {noreply, state()});
                 ?CALL_SPEC(get_requests,                       {reply, [requestMessage()], state()});
                 ?CALL_SPEC(get_notifications,                  {reply, [notificationMessage()], state()}).
handle_call({send_request, Method, Params}, From,
            #state{port = Port, request_id = RequestId, pending = Pending} = State) ->
  EffectiveParams = maps:merge(default_params(Method), Params),
  Content = rebar3_bsp_protocol:request(RequestId, Method, EffectiveParams),
  ok = rebar3_bsp_protocol:send_message(Port, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending = [{RequestId, From}|Pending]
                       }};
handle_call(get_requests, _From, #state{ requests = Requests } = State) ->
  {reply, lists:reverse(Requests), State#state{ requests = [] }};
handle_call(get_notifications, _From, #state{ notifications = Notifications } = State) ->
  {reply, lists:reverse(Notifications), State#state{ notifications = [] }}.

-spec handle_cast({send_notification, binary(), params()}, state()) -> {noreply, state()}.
handle_cast({send_notification, Method, Params}, #state{port = Port} = State) ->
  EffectiveParams = maps:merge(default_params(Method), Params),
  Content = rebar3_bsp_protocol:notification(Method, EffectiveParams),
  ok = rebar3_bsp_protocol:send_message(Port, Content),
  {noreply, State}.

-spec handle_info({port() | pid(), {data, binary()}}, state()) -> {noreply, state(), {continue, decode}};
                 ({port() | pid(), {exit_status, integer()}}, state()) -> {stop, {exit_status, integer()}, state()}.
handle_info({Port, {data, NewData}}, #state{port = Port, buffer = OldBuffer} = State) ->
  {noreply, State#state{ buffer = <<OldBuffer/binary, NewData/binary>> }, {continue, decode}};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
  {stop, {exit_status, Status}, State#state{port = undefined}}.

-spec handle_continue(decode, state()) -> {noreply, state(), {continue, messages}} | {stop, state(), {decode_error, any()}};
                     (messages, state()) -> {noreply, state()} | {noreply, state(), {continue, messages}}.
handle_continue(decode, #state{buffer = Buffer, messages = OldMsgs} = State) ->
  case rebar3_bsp_protocol:decode_packets(Buffer) of
    {ok, NewMsgs, RestData} ->
      {noreply, State#state{ buffer = RestData, messages = OldMsgs ++ NewMsgs }, {continue, messages}};
    {error, Reason} ->
      {stop, State, {decode_error, Reason}}
  end;
handle_continue(messages, #state{ messages = [] } = State) ->
  {noreply, State};
handle_continue(messages, State) ->
  {noreply, handle_message(State), {continue, messages}}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec start_link_impl({open_port, any(), [any()]} | {reuse_port, port() | pid()}) -> {ok, pid()}.
start_link_impl(PortSpec) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, PortSpec, []).

-spec handle_message(state()) -> state().
handle_message(#state{messages = [M|Ms]} = State) ->
  MessageType = rebar3_bsp_protocol:message_type(M),
  handle_message(MessageType, M, State#state{ messages = Ms }).

-spec handle_message(request | response | notification, map(), state()) -> state().
handle_message(request, M, #state{ requests = Requests} = State) ->
  State#state{ requests = [M|Requests] };
handle_message(notification, M, #state{ notifications = Notifications } = State) ->
  State#state{ notifications = [M|Notifications] };
handle_message(response, #{ id := Id } = M, #state{ pending = Pending } = State) ->
  NewPending = case proplists:get_value(Id, Pending) of
                 undefined ->
                   ?LOG_WARNING("Discarding unexpected response ~p", [M]),
                   Pending;
                 From ->
                   gen_server:reply(From, M),
                   proplists:delete(Id, Pending)
               end,
  State#state{ pending = NewPending }.

-spec default_params(binary()) -> params().
default_params(<<"build/initialize">>) ->
  #{ displayName  => <<"Rebar3 BSP Client">>
   , version      => rebar3_bsp_connection:version(?BSP_APPLICATION)
   , bspVersion   => ?BSP_VSN
   , capabilities => #{ languageIds => [<<"erlang">>] }
   };
default_params(Method) when is_binary(Method) ->
  #{}.

-spec ensure_binary(atom() | binary()) -> binary().
ensure_binary(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom);
ensure_binary(Binary) when is_binary(Binary) ->
  Binary.

-spec unpeel_response(map()) -> {ok, map()} | {error, map()}.
unpeel_response(#{ error := Error }) ->
  {error, Error};
unpeel_response(#{ result := Result}) ->
  {ok, Result}.

