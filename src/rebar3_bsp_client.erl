%%==============================================================================
%% A client for the Build Server Protocol using the STDIO transport
%%==============================================================================
-module(rebar3_bsp_client).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).
%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        , terminate/2
        ]).

%%==============================================================================
%% Exports
%%==============================================================================
%% Erlang API
-export([ start_link/1
        , stop/0
        ]).

-export([ send_request/2
        , send_notification/2
        , get_requests/0
        , get_notifications/0
        ].

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
-record(state, { request_id    = 1 :: request_id()
               , pending       = []
               , notifications = []
               , requests      = []
               , port          :: port() | pid()
               , buffer        = <<>>
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()      :: #state{}.
-type request_id() :: pos_integer().
-type params()     :: map().

%%==============================================================================
%% Erlang API
%%==============================================================================
start_link({root, RootPath}) ->
  {ok, Executable, Args} = rebar3_bsp_connection:discover(RootPath),
  start_link({exec, Executable, Args});
start_link({exec, Executable, Args}) ->
  Port = open_port({spawn_executable, Executable},
                   [{args, Args}, use_stdio, binary]),
  start_link({port, Port});
start_link({port, Port}) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {port, Port}, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec send_request(binary(), map()) -> {ok, request_id()}.
send_request(Method, Params) ->
  gen_server:call(?SERVER, {send_request, Method, Params})

-spec send_notification(binary(), map()) -> ok.
send_notification(Method, Params) ->
  gen_server:call(?SERVER, {send_notification, Method, Params}).

-spec get_requests() -> [map()].
get_requests() ->
  gen_server:call(?SERVER, get_requests).

-spec get_notifications() -> [map()].
get_notifications() ->
  gen_server:call(?SERVER, get_notifications).

%%==============================================================================
%% Server Lifetime
%%==============================================================================

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({port, port() | pid()}) -> {ok, state()}.
init({port, Port}) ->
  {ok, #state{port = Port}}.

-spec handle_call({send_request, binary(), params()}, pid(), state()) -> {noreply, {ok, integer()}, state()};
                 ({send_notification, binary(), params()}, pid(), state()) -> {reply, ok, state()};
                 (get_requests, pid(), state()) -> {reply, [requestMessage()], state()};
                 (get_notifications, pid(), state()) -> {reply, [notificationMessage()], state()}.
handle_call({send_request, Method, Params}, From,
            #state{port = Port, request_id = RequestId, pending = Pending} = State) ->
  EffectiveParams = maps:merge(default_params(Method), Params),
  Content = rebar3_bsp_protocol:request(RequestId, Method, EffectiveParams),
  send(Port, Content),
  {noreply, {ok, RequestId}, State#state{ request_id = RequestId + 1
                                        , pending = [{RequestId, From}|Pending]
                                        }};
handle_call({send_notification, Method, Params}, _From,
            #state{port = Port} = State) ->
  EffectiveParams = maps:merge(default_params(Method), Params),
  Content = rebar3_bsp_protocol:notification(Method, EffectiveParams),
  send(Port, Content),
  {reply, ok, State};
handle_call(get_requests, _From, #state{ requests = Requests } = State) ->
  {reply, lists:reverse(Requests), State#state{ requests = [] }};
handle_call(get_notifications, _From, #state{ notifications = Notifications } = State) ->
  {reply, lists:reverse(Notifications), State#state{ notifications = [] }}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  error(badarg).

-spec handle_info(any(), state()) -> {noreply, state(), {continue, {messages, [message()]}}}.
handle_info({Port, {data, Data}}, #state{port = Port, buffer = Buffer} = State) ->
  AllData = <<Buffer/binary, Data/binary>>,
  {ok, Messages, RestData} = rebar3_bsp_jsonrpc:decode_packets(AllData),
  {noreply, State#state{ buffer = RestData }, {continue, {messages, Messages}}}.

-spec handle_continue({messages, []}, state()) -> {noreply, state()};
                     ({messages, [message()]}, state()) -> {noreply,
                                                            state(),
                                                            {continue, {messages, [message()]}}.
handle_continue({messages, []}, State) ->
  {noreply, State};
handle_continue({messages, [M|Ms]}, State) ->
  {noreply, handle_message(M, State), {continue, {messages, Ms}}}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{port = Port} = _State) when is_port(Port) ->
  true = port_close(Port),
  ok;
terminate(_Reason, _State) ->
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_message(message(), state()) -> state().
handle_message(M, State) ->
  case M of
    %% Request - always has id and method
    #{ id := _Id, method := _Method } ->
      handle_request(M, State);
    %% Response - always has id but never has a method
    #{ id := _Id } ->
      handle_response(M, State);
    %% Notification - doesn't have an id, always has a method
    #{ method := _Method } ->
      handle_notification(M, State)
  end.  

-spec handle_request(requestMessage(), state()) -> state().
handle_request(M, #state{ requests = Requests} = State) ->
  State#state{ requests = [M|Requests] }.

-spec handle_response(responseMessage(), state()) -> state().
handle_response(#{ id := Id } = M, #state{ pending = Pending } = State) ->
  NewPending = case proplists:get_value(Id, Pending) of
                 undefined ->
                   ?LOG_WARNING("Discarding unexpected response ~p", [M]),
                   Pending;
                 From ->
                   gen_server:reply(From, M),
                   proplists:delete(Id, Pending)
               end,
  State#state{ pending = NewPending }.

-spec handle_notification(notificationMessage(), state()) -> state().
handle_notification(M, #state{ notifications = Notifications } = State) ->
  State#state{ notifications = [M|Notifications] }.

-spec default_params(binary()) -> params().
default_params(<<"build/initialize">>) ->
  #{ displayName  => <<"Rebar3 BSP Client">>
   , version      => rebar3_bsp_connection:version(?BSP_APPLICATION)
   , bspVersion   => ?BSP_VSN
   , capabilities => #{ languageIds => [<<"erlang">>] }
   };
default_params(_Method) ->
  #{}.

-spec send(port() | pid(), binary()) -> ok.
send(Port, Payload) ->
  Port ! {self(), {command, Payload}},
  ok.

