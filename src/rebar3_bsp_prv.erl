-module(rebar3_bsp_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, bsp).
-define(DEPS, [compile]).
-define(AGENT, rebar3_bsp_server).
-define(NAME_PREFIX, "rebar3_bsp_").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Opts = [{generate, undefined, "generate", boolean, "Generate the BSP Connection file and exit"}],
  Provider = providers:create([
                               {name, ?PROVIDER},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, ?DEPS},
                               {example, "rebar3 bsp"},
                               {opts, Opts},
                               {short_desc, "Build Server Protocol (BSP) plugin"},
                               {desc, "Plugin adding Build Server Protocol (BSP) support for rebar3"},
                               {profiles, [test]}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  case proplists:is_defined(generate, Args) of
    true ->
      Dir = rebar_state:dir(State),
      rebar3_bsp_connection:generate(Dir);
    false ->
      setup_name(State),
      start_agent(State)
  end,
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec start_agent(rebar_state:t()) -> no_return().
start_agent(State) ->
  simulate_proc_lib(),
  true = register(?AGENT, self()),
  {ok, GenState} = rebar3_bsp_server:init(State),
  gen_server:enter_loop(rebar3_bsp_server, [], GenState, {local, ?AGENT}, hibernate).

-spec setup_name(rebar_state:t()) -> ok.
setup_name(State) ->
  {_Long, Short, Opts} = rebar_dist_utils:find_options(State),
  Name = case Short of
           undefined ->
             list_to_atom(filename:basename(rebar_state:dir(State)));
           N ->
             N
         end,
  Int = erlang:phash2(erlang:timestamp()),
  Id = lists:flatten(io_lib:format("~s_~p_~p", [?NAME_PREFIX, Name, Int])),
  rebar_dist_utils:short(list_to_atom(Id), Opts),
  ok.

-spec simulate_proc_lib() -> ok.
simulate_proc_lib() ->
  FakeParent = spawn_link(fun() -> timer:sleep(infinity) end),
  put('$ancestors', [FakeParent]),
  put('$initial_call', {rebar3_bsp_server, init, 1}),
  ok.
