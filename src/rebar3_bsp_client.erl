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
-record(state, { request_id    = 1 :: request_id()
               , pending       = []
               , notifications = []
               , requests      = []
               , messages      = []
               , buffer        = <<>>
               , port              :: port() | pid() | undefined
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()      :: #state{}.
-type request_id() :: pos_integer().

%%==============================================================================
%% Erlang API
%%==============================================================================
start_link({root, RootPath}) ->
  {ok, Executable, Args} = rebar3_bsp_connection:discover(RootPath),
  start_link({exec, Executable, Args, [{cd, RootPath}]});
start_link({exec, Executable, Args, PortSettings}) ->
  start_link({ open_port
             , {spawn_executable, Executable}
             , PortSettings ++ [{args, Args}, use_stdio, binary, exit_status]
             });
start_link({port, Port}) ->
  start_link({reuse_port, Port});
start_link(PortSpec) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, PortSpec, []).

stop() ->
  gen_server:call(?SERVER, stop).

send_request(Method, Params) ->
  gen_server:call(?SERVER, {send_request, Method, Params}).

send_notification(Method, Params) ->
  gen_server:call(?SERVER, {send_notification, Method, Params}).

get_requests() ->
  gen_server:call(?SERVER, get_requests).

get_notifications() ->
  gen_server:call(?SERVER, get_notifications).

-spec init({open_port | reuse_port, any()}) -> {ok, state()}.
init(PortSpec) ->
  Port = case PortSpec of
           {open_port, PortName, PortSettings} ->
             erlang:open_port(PortName, PortSettings);
           {reuse_port, P} ->
             P
         end,
  {ok, #state{port = Port}}.

handle_call({send_request, Method, Params}, From,
            #state{port = Port, request_id = RequestId, pending = Pending} = State) ->
  EffectiveParams = maps:merge(default_params(Method), Params),
  Content = rebar3_bsp_protocol:request(RequestId, Method, EffectiveParams),
  ok = rebar3_bsp_protocol:send_message(Port, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending = [{RequestId, From}|Pending]
                       }};
handle_call({send_notification, Method, Params}, _From,
            #state{port = Port} = State) ->
  EffectiveParams = maps:merge(default_params(Method), Params),
  Content = rebar3_bsp_protocol:notification(Method, EffectiveParams),
  ok = rebar3_bsp_protocol:send_message(Port, Content),
  {reply, ok, State};
handle_call(get_requests, _From, #state{ requests = Requests } = State) ->
  {reply, lists:reverse(Requests), State#state{ requests = [] }};
handle_call(get_notifications, _From, #state{ notifications = Notifications } = State) ->
  {reply, lists:reverse(Notifications), State#state{ notifications = [] }};
handle_call(stop, _From, State) ->
  {stop, normal, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, _State) ->
  error(badarg).

-spec handle_info(any(), state()) -> {noreply, state(), {continue, {messages, [message()]}}}.
handle_info({Port, {data, NewData}}, #state{port = Port, buffer = OldBuffer} = State) ->
  {noreply, State#state{ buffer = <<OldBuffer/binary, NewData/binary>> }, {continue, decode}};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
  {stop, {exit_status, Status}, State#state{port = undefined}}.

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
default_params(_Method) ->
  #{}.

