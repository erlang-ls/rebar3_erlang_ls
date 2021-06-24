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
        , post_message/1
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
        ]).
%%==============================================================================
%% Includes
%%==============================================================================
-include("rebar3_bsp.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT, 60 * 1000).

-define(CALL_SPEC(Request, Result), (Request, from(), state()) -> Result).
-define(CAST_SPEC(Request, Result), (Request, state()) -> Result).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { request_id    = 1    :: request_id()
               , pending       = []   :: [{request_id(), from()}]
               , notifications = []   :: [notificationMessage()]
               , requests      = []   :: [requestMessage()]
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
  RequestId = send_request('build/shutdown', null),
  ok = send_notification('build/exit', null),
  receive_response(RequestId, ?TIMEOUT),
  gen_server:stop(?SERVER).

-spec post_message(map()) -> ok.
post_message(Message) ->
  gen_server:cast(?SERVER, {incoming_message, Message}).

-spec send_request(method(), params()) -> any().
send_request(Method, Params) ->
  gen_server:send_request(?SERVER, {send_request, rebar3_bsp_util:ensure_binary(Method), Params}).

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
  gen_server:cast(?SERVER, {send_notification, rebar3_bsp_util:ensure_binary(Method), Params}).

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

-spec handle_call?CALL_SPEC({send_request, binary(), params()}, {noreply, state()});
                 ?CALL_SPEC(get_requests,                       {reply, [requestMessage()], state()});
                 ?CALL_SPEC(get_notifications,                  {reply, [notificationMessage()], state()}).
handle_call({send_request, Method, Params}, From,
            #state{port = Port, request_id = RequestId, pending = Pending} = State) ->
  EffectiveParams = effective_params(Method, Params),
  Content = rebar3_bsp_protocol:request(RequestId, Method, EffectiveParams),
  ok = rebar3_bsp_protocol:send_message(Port, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending = [{RequestId, From}|Pending]
                       }};
handle_call(get_requests, _From, #state{ requests = Requests } = State) ->
  {reply, lists:reverse(Requests), State#state{ requests = [] }};
handle_call(get_notifications, _From, #state{ notifications = Notifications } = State) ->
  {reply, lists:reverse(Notifications), State#state{ notifications = [] }}.

-spec handle_cast?CAST_SPEC({send_notification, binary(), params()}, {noreply, state()});
                 ?CAST_SPEC({incoming_message, map()},               {noreply, state()}).
handle_cast({send_notification, Method, Params}, #state{port = Port} = State) ->
  EffectiveParams = effective_params(Method, Params),
  Content = rebar3_bsp_protocol:notification(Method, EffectiveParams),
  ok = rebar3_bsp_protocol:send_message(Port, Content),
  {noreply, State};
handle_cast({incoming_message, Message}, State) ->
  {noreply, handle_message(Message, State)}.

-spec handle_info({port() | pid(), {data, binary()}}, state())         -> {noreply, state()};
                 ({port() | pid(), {exit_status, integer()}}, state()) -> {stop, {shutdown, any()}, state()};
                 ({'EXIT', port() | pid(), normal}, state())           -> {stop, normal, state()}.
handle_info({Port, {data, NewData}}, #state{port = Port, buffer = OldBuffer} = State) ->
  {Messages, RestData} = rebar3_bsp_protocol:carefully_decode_packets(<<OldBuffer/binary, NewData/binary>>),
  [ ok = post_message(M) || M <- Messages ],
  { noreply, State#state{ buffer = RestData }};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
  {stop, {shutdown, {exit_status, Status}}, State#state{port = undefined}};
handle_info({'EXIT', Port, normal}, #state{port = Port} = State) ->
  {stop, normal, State#state{port = undefined}}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec start_link_impl({open_port, any(), [any()]} | {reuse_port, port() | pid()}) -> {ok, pid()}.
start_link_impl(PortSpec) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, PortSpec, []).

-spec handle_message(map(), state()) -> state().
handle_message(Message, #state{ requests = Requests
                              , notifications = Notifications
                              , pending = Pending } = State) ->
  case rebar3_bsp_protocol:message_type(Message) of
    request ->
      State#state{ requests = [Message|Requests] };
    notification ->
      State#state{ notifications = [Message|Notifications] };
    response ->
      Id = rebar3_bsp_protocol:message_id(Message),
      case proplists:get_value(Id, Pending) of
        undefined ->
          ?LOG_WARNING("Discarding unexpected response ~p", [Message]),
          State;
        From ->
          gen_server:reply(From, Message),
          State#state{ pending = proplists:delete(Id, Pending) }
      end
  end.

-spec effective_params(binary(), map() | null) -> map() | null.
effective_params(Method, Params) when is_map(Params) ->
  maps:merge(default_params(Method), Params);
effective_params(_Method, null) ->
  null.

-spec default_params(binary()) -> params().
default_params(<<"build/initialize">>) ->
  #{ displayName  => <<"Rebar3 BSP Client">>
   , version      => rebar3_bsp_connection:version(?BSP_APPLICATION)
   , bspVersion   => ?BSP_VSN
   , capabilities => #{ languageIds => [<<"erlang">>] }
   };
default_params(Method) when is_binary(Method) ->
  #{}.

-spec unpeel_response(map()) -> {ok, map()} | {error, map()}.
unpeel_response(#{ error := Error }) ->
  {error, Error};
unpeel_response(#{ result := Result}) ->
  {ok, Result}.
