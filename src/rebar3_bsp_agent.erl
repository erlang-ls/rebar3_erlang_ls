%%==============================================================================
%% A BSP agent, based on a stripped down version of rebar3's rebar_agent
%%
%% Runs a process that holds a rebar3 state and can be used to
%% statefully maintain loaded project state into a running VM.
%%==============================================================================
-module(rebar3_bsp_agent).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([ start_link/1
        , stop/0
        , run_ct_test/2
        , run_xref/0
        ]).

%% BSP Callbacks
-export([ handle_message/1
        , handle_notification/2
        , handle_request/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , format_status/2
        ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #{ rebar3_state := rebar3_state:t()
                  , stdio_server := pid()
                  }.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(rebar3_state:t()) -> {ok, pid()}.
start_link(R3State) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, R3State, []).

-spec stop() -> ok.
stop() ->
  gen_server:call(?SERVER, stop).

-spec run_ct_test(atom(), atom()) -> ok.
run_ct_test(Suite, Case) ->
  Groups = string:join([atom_to_list(G) || G <- ct_groups(Suite, Case)], ","),
  Args = [ io_lib:format("--suite=~s", [Suite])
         , io_lib:format("--case=~s", [Case])
         , io_lib:format("--group=~s", [Groups])
         ],
  gen_server:call(?SERVER, {run_ct_test, Args}, infinity).

-spec run_xref() -> ok.
run_xref() ->
  gen_server:call(?SERVER, {run_xref}, infinity).

%%==============================================================================
%% BSP Callbacks
%%==============================================================================
-spec handle_message(map()) -> ok | {response, binary()}.
handle_message(#{method := Method, params := Params} = Message) ->
  case Message of
    #{id := Id} ->
      Result = handle_request(Method, Params),
      Response = rebar3_bsp_protocol:response(Id, Result),
      {response, Response};
    _ ->
      ok = handle_notification(Method, Params),
      ok
  end.

-spec handle_notification(binary(), map()) -> ok.
handle_notification(Method, Params) ->
  gen_server:cast(?SERVER, {notification, Method, Params}).

-spec handle_request(binary(), map()) -> map().
handle_request(Method, Params) ->
  gen_server:call(?SERVER, {request, Method, Params}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(rebar3_state:t()) -> {ok, state()}.
init(R3State) ->
  Dir = rebar_state:dir(R3State),
  case rebar3_bsp_connection:exists(Dir) of
    true ->
      rebar_log:log(debug, "Using existing connection file from: ~s", [Dir]),
      ok;
    false ->
      rebar_log:log(debug, "Generating new connection file in: ~s", [Dir]),
      rebar3_bsp_connection:generate(Dir)
  end,
  {ok, StdIOServer} = rebar3_bsp_stdio:start_link(),
  {ok, #{ rebar3_state => R3State
        , stdio_server => StdIOServer}}.

-spec handle_call(any(), any(), state()) ->
        {reply, any(), state()} | {noreply, state()}.
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({run_ct_test, Args}, From, State) ->
  #{ rebar3_state := R3State } = State,
  R3State1 = update_state_for_ct(R3State, From),
  %% The ct command supports a mode where it doesn't recompile, and therefore does
  %% not depend on the compile pass. Invoke compilation explicitly.
  rebar_log:log(debug, "Compiling code for CT", []),
  try rebar3:run(R3State1, ["compile"]) of
    {ok, _} ->
      rebar_log:log(debug, "Running CT test ~p", [Args]),
      try rebar3:run(R3State1, ["ct"|Args]) of
        {ok, _} ->
          %% rebar3 unset the paths on success, causing the second run to fail.
          %% Let's revert that.
          %% TODO: Report and link issue
          ok = rebar_paths:set_paths([deps, plugins], R3State1),
          {reply, ok, State#{ rebar3_state => R3State1 }};
        {error, Error} ->
          rebar_log:log(debug, "Running CT tests failed: ~p", [Error]),
          %% The CT Hook will take care of returning a more meaningful error
          {noreply, State#{ rebar3_state => R3State1 }}
      catch C:E:S ->
          rebar_log:log(debug, "Running CT failed", []),
          {reply, {error, {C, E, S}}, State#{ rebar3_state => R3State1 }}
      end
  catch C:E:S ->
      rebar_log:log(debug, "Compilation for CT failed", []),
      {reply, {error, {C, E, S}}, State#{ rebar3_state => R3State1 }}
  end;
handle_call({run_xref}, _From, State) ->
  #{ rebar3_state := R3State } = State,
  rebar_log:log(debug, "Running XRef", []),
  Result = case rebar3:run(R3State, ["xref"]) of
             {ok, _} ->
               rebar_log:log(debug, "XRef analysis completed", []),
               %% rebar3 unset the paths on success, causing the second run to fail.
               %% Let's revert that.
               %% TODO: Report and link issue
               ok = rebar_paths:set_paths([deps, plugins], R3State),
               ok;
             {error, Error} ->
               rebar_log:log(debug, "Error running XRef analysis ~p", [Error]),
               {error, Error}
           end,
  {reply, Result, State};
handle_call({request, Method, Params}, _From, State) ->
  #{ rebar3_state := R3State } = State,
  Function = dispatch(Method),
  {Result, NewR3State} = rebar3_bsp_methods:Function(Params, R3State),
  {reply, Result, State#{ rebar3_state => NewR3State }};
handle_call(Request, _From, State) ->
  rebar_log:log(debug, "Unexpected request: ~p", [Request]),
  {reply, {error, {unexpected_request, Request}}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({notification, _Method, _Params}, State) ->
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({result, From, Result}, State) ->
  rebar_log:log(debug, "Received result: ~p", [Result]),
  gen_server:reply(binary_to_term(From), Result),
  {noreply, State};
handle_info({'DOWN', _Ref, process, _Pid, normal}, State) ->
  {noreply, State};
handle_info(Request, State) ->
  rebar_log:log(debug, "Unexpected handle_info request: ~p", [Request]),
  {noreply, State}.

format_status(_Opt, [_PDict, State]) ->
  [{data, [{"State", State#{rebar3_state => redacted}}]}].

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec update_state_for_ct(rebar_state:t(), {pid(), reference()}) ->
        rebar_state:t().
update_state_for_ct(State, From) ->
  disable_cover(inject_ct_hook(State, From)).

-spec disable_cover(rebar_state:t()) -> rebar_state:t().
disable_cover(State) ->
  rebar_state:set(State, cover_enabled, false).

-spec inject_ct_hook(rebar_state:t(), {pid(), reference()}) -> rebar_state:t().
inject_ct_hook(State, From) ->
  Opts0 = rebar_state:get(State, ct_opts, []),
  Hooks0 = proplists:get_value(ct_hooks, Opts0, []),
  Hook = {rebar3_bsp_ct_hook, #{from => term_to_binary(From)}},
  Hooks = lists:keystore(rebar3_bsp_ct_hook, 1, Hooks0, Hook),
  Opts = lists:keystore(ct_hooks, 1, Opts0, {ct_hooks, Hooks}),
  rebar_state:set(State, ct_opts, Opts).

-spec ct_groups(atom(), atom()) -> [atom()].
ct_groups(Suite, Case) ->
  %% Rebar3 purges code between runs, so let's ensure the module is
  %% loaded
  code:ensure_loaded(Suite),
  case erlang:function_exported(Suite, groups, 0) of
    true ->
      [Group || {Group, _Prop, Cases} <-
                  Suite:groups(), lists:member(Case, Cases)];
    false ->
      []
  end.

-spec dispatch(binary()) -> atom().
dispatch(Method) ->
  Replaced = string:replace(Method, <<"/">>, <<"_">>),
  Lower = string:lowercase(Replaced),
  Binary = unicode:characters_to_binary(Lower),
  binary_to_atom(Binary, utf8).
