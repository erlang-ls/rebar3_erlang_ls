-module(rebar3_bsp_echo_port).
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% API
%%==============================================================================
-export([ start_link/0
        , stop/1
        , set_endpoints/2
        , sync/1
        ]).

%%==============================================================================
%% Callbacks
%%==============================================================================
-export([ init/1
        , system_terminate/4
        , system_continue/3
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type endpoints() :: {pid(), pid()}.
-type debug_opts() :: [sys:dbg_opt()].
-type state() :: #{ parent := pid()
                  , endpoints := undefined | endpoints()
                  , deb := debug_opts()
                  }.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  {ok, Pid} = proc_lib:start_link(?MODULE, init, [self()]),
  {ok, Pid}.

-spec stop(pid()) -> ok.
stop(Pid) ->
  proc_lib:stop(Pid).

-spec set_endpoints(pid(), {pid(), pid()}) -> ok.
set_endpoints(Pid, {A, B}) when is_pid(A), is_pid(B) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, {set_endpoints, {A, B}}},
  receive
    {Ref, ok} ->
      ok
  end.

-spec sync(pid()) -> ok.
sync(Pid) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, sync},
  receive
    {Ref, sync_ok} ->
      ok
  end.

%%==============================================================================
%% Callbacks
%%==============================================================================
-spec init(pid()) -> no_return().
init(Parent) ->
  ok = proc_lib:init_ack({ok, self()}),
  Deb = sys:debug_options([]),
  loop(#{ parent => Parent, endpoints => undefined, deb => Deb }).

-spec system_terminate(any(), pid(), debug_opts(), state()) -> no_return().
system_terminate(Reason, Parent, Deb, State) ->
  #{ parent := Parent, deb := Deb } = State, %% sanity check
  terminate(Reason, State).

-spec system_continue(pid(), debug_opts(), state()) -> no_return().
system_continue(Parent, Deb, State) ->
  #{ parent := Parent, deb := Deb } = State, %% sanity check
  loop(State).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec loop(state()) -> no_return().
loop(State) ->
  receive
    {From, Ref, {set_endpoints, {A, B}}} when map_get(endpoints, State) =:= undefined ->
      handle_set_endpoints(From, Ref, {A, B}, State);
    {From, {command, Data}} when map_get(endpoints, State) =/= undefined ->
      handle_command(From, Data, State);
    {From, close} ->
      handle_close(From, State);
    {From, Ref, sync} ->
      handle_sync(From, Ref, State);
    {system, From, Request} ->
      handle_system_msg(From, Request, State)
  end.

-spec handle_set_endpoints(pid(), any(), {pid(), pid()}, state()) -> no_return().
handle_set_endpoints(From, Ref, {A, B}, #{ endpoints := undefined } = State) when is_pid(A) andalso is_pid(B) ->
  From ! {Ref, ok},
  loop(State#{ endpoints => {A, B} }).

-spec handle_command(pid(), any(), state()) -> no_return().
handle_command(From, Data, State) ->
  {_Sender, Receiver} = get_sender_receiver(From, State),
  Receiver ! {self(), {data, Data}},
  loop(State).

-spec handle_close(pid(), state()) -> no_return().
handle_close(_From, #{ endpoints := undefined } = _State) ->
  exit(closed);
handle_close(From, State) ->
  {Sender, _Receiver} = get_sender_receiver(From, State),
  Sender ! {self(), closed},
  terminate(closed, State).

-spec handle_sync(pid(), any(), state()) -> no_return().
handle_sync(From, Ref, State) ->
  case State of
    #{ endpoints := {A, B} } ->
      A ! {self(), {data, <<>>}},
      B ! {self(), {data, <<>>}},
      ok;
    #{ endpoints := undefined } ->
      ok
  end,
  From ! {Ref, sync_ok},
  loop(State).

-spec handle_system_msg(any(), any(), state()) -> no_return().
handle_system_msg(From, Request, State) ->
  #{ parent := Parent, deb := Deb } = State,
  sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State).

-spec terminate(any(), state()) -> no_return().
terminate(Reason, State) ->
  #{ endpoints := Endpoints } = State,
  case Endpoints of
    {A, B} ->
      send_exit(A, Reason),
      send_exit(B, Reason);
    undefined ->
      ok
  end,
  exit(Reason).

-spec send_exit(pid(), any()) -> ok.
send_exit(To, Reason) when is_pid(To) ->
  To ! {'EXIT', self(), Reason},
  ok.

-spec get_sender_receiver(pid(), state()) -> {pid(), pid()}.
get_sender_receiver(From, State) ->
  #{ endpoints := {A, B} } = State,
  case From of
    A ->
      {A, B};
    B ->
      {B, A}
  end.

