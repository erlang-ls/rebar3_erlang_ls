%%==============================================================================
%% Top Level Supervisor
%%==============================================================================
-module(rebar3_bsp_sup).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(supervisor).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ start_link/0 ]).

%% Supervisor Callbacks
-export([ init/1 ]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%==============================================================================
%% supervisors callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => rest_for_one
              , intensity => 5
              , period    => 60
              },
  simulate_group_leader(),
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

-spec simulate_group_leader() -> ok.
simulate_group_leader() ->
  GL = erlang:group_leader(),
  application:set_env(rebar3_bsp, group_leader, GL),
  Pid = spawn_link(fun noop_group_leader/0),
  true = erlang:group_leader(Pid, self()),
  ok.

%% @doc Simulate a group leader but do nothing
-spec noop_group_leader() -> no_return().
noop_group_leader() ->
  receive
    Message ->
      case Message of
        {io_request, From, ReplyAs, getopts} ->
          From ! {io_reply, ReplyAs, []};
        {io_request, From, ReplyAs, _} ->
          From ! {io_reply, ReplyAs, ok};
        _ ->
          ok
      end,
      noop_group_leader()
  end.
