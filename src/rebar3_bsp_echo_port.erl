-module(rebar3_bsp_echo_port).
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% API
%%==============================================================================
-export([ start_link/0
        , stop/1
        , set_endpoints/2
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

%%==============================================================================
%% Callbacks
%%==============================================================================
-spec init(pid()) -> no_return().
init(Parent) ->
  ok = proc_lib:init_ack({ok, self()}),
  Deb = sys:debug_options([]),
  loop(undefined, Parent, Deb).

-spec system_terminate(any(), pid(), debug_opts(), endpoints() | undefined) -> no_return().
system_terminate(Reason, _Parent, _Deb, Endpoints) ->
  terminate(Endpoints, Reason).

-spec system_continue(pid(), debug_opts(), endpoints() | undefined) -> no_return().
system_continue(Parent, Deb, Endpoints) ->
  loop(Endpoints, Parent, Deb).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec loop(endpoints() | undefined, pid(), debug_opts()) -> no_return().
loop(undefined, Parent, Deb) ->
  receive
    {From, Ref, {set_endpoints, {A, B}}} ->
      handle_set_endpoints(From, Ref, {A, B}, Parent, Deb);
    {From, close} ->
      handle_close(From, undefined, Parent, Deb);
    {system, From, Request} ->
      handle_system_msg(From, Request, undefined, Parent, Deb)
  end;
loop({A, B}, Parent, Deb) when is_pid(A) andalso is_pid(B) ->
  receive
    {From, {command, Data}}  ->
      handle_command(From, Data, {A, B}, Parent, Deb);
    {From, close} ->
      handle_close(From, {A, B}, Parent, Deb);
    {system, From, Request} ->
      handle_system_msg(From, Request, {A, B}, Parent, Deb)
  end.

-spec handle_set_endpoints(pid(), any(), endpoints(), pid(), debug_opts()) -> no_return().
handle_set_endpoints(From, Ref, {A, B}, Parent, Deb) ->
  case is_pid(A) andalso is_pid(B) of
    true ->
      From ! {Ref, ok},
      loop({A, B}, Parent, Deb);
    false ->
      error(badarg)
  end.

-spec handle_command(pid(), any(), endpoints(), pid(), debug_opts()) -> no_return().
handle_command(From, Data, {A, B}, Parent, Deb) ->
  {_Sender, Receiver} = get_sender_receiver({A, B}, From),
  Receiver ! {self(), {data, Data}},
  loop({A, B}, Parent, Deb).

-spec handle_close(pid(), endpoints() | undefined, pid(), debug_opts()) -> no_return().
handle_close(From, {A, B}, _Parent, _Deb) ->
  {Sender, _Receiver} = get_sender_receiver({A, B}, From),
  Sender ! {self(), closed},
  terminate({A, B}, closed);
handle_close(_From, undefined, _Parent, _Deb) ->
  exit(closed).

-spec handle_system_msg({pid(), any()}, any(), endpoints() | undefined, pid(), debug_opts()) -> no_return().
handle_system_msg(From, Request, Endpoints, Parent, Deb) ->
  sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, Endpoints).

-spec terminate(endpoints() | undefined, any()) -> no_return().
terminate(Endpoints, Reason) ->
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

-spec get_sender_receiver(endpoints(), pid()) -> {pid(), pid()}.
get_sender_receiver({A, B}, From) ->
  case From of
    A ->
      {A, B};
    B ->
      {B, A}
  end.

