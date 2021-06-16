-module(rebar3_bsp_methods_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ build_initialize/1
        , build_initialized/1
        , workspace_buildtargets/1
        , buildtarget_compile/1
        , buildtarget_sources/1
        , buildtarget_dependencysources/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:load(rebar3_bsp),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  file:set_cwd(?config(cwd, Config)),
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  {ok, Cwd} = file:get_cwd(),
  ok = file:set_cwd(sample_app_dir()),
  {ok, RebarConfig} = file:consult("rebar.config"),
  {ok, State} = erl_subgraph_compile:init(rebar_state:new(RebarConfig)),
  {ok, _Pid} = rebar3_bsp_agent:start_link(State),
  [{cwd, Cwd}|Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  ok = rebar3_bsp_agent:stop(),
  ok.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

%%==============================================================================
%% Testcases
%%==============================================================================
-spec build_initialize(config()) -> ok.
build_initialize(_Config) ->
  Result = rebar3_bsp_agent:handle_request(<<"build/initialize">>, #{}),
  ?assertEqual( #{ bspVersion => <<"2.0.0">>
                 , capabilities => #{}
                 , displayName => <<"rebar3_bsp">>
                 , version => <<"0.1.0">>
                 }, Result),
  ok.

-spec build_initialized(config()) -> ok.
build_initialized(_Config) ->
  Result = rebar3_bsp_agent:handle_request(<<"build/initialized">>, #{}),
  ?assertEqual(ok, Result),
  ok.

-spec workspace_buildtargets(config()) -> ok.
workspace_buildtargets(_Config) ->
  Result = rebar3_bsp_agent:handle_request(<<"workspace/buildTargets">>, #{}),
  ?assertEqual(#{ targets => [#{ id => <<"default">>}] }, Result),
  ok.

-spec buildtarget_compile(config()) -> ok.
buildtarget_compile(_Config) ->
  Result = rebar3_bsp_agent:handle_request(<<"buildTarget/compile">>, #{}),
  ?assertEqual(#{ statusCode => 0 }, Result),
  ok.

-spec buildtarget_sources(config()) -> ok.
buildtarget_sources(_Config) ->
  %% Ensure the default build target is initialized
  _Result = rebar3_bsp_agent:handle_request(<<"build/initialized">>, #{}),
  Params = #{ targets => [<<"default">>] },
  Result = rebar3_bsp_agent:handle_request(<<"buildTarget/sources">>, Params),
  {ok, Cwd} = file:get_cwd(),
  ?assertEqual(#{ items => [filename:join([Cwd, "_build", "default", "lib", "sample"])] }, Result),
  ok.

-spec buildtarget_dependencysources(config()) -> ok.
buildtarget_dependencysources(_Config) ->
  %% Ensure the default build target is initialized
  _Result = rebar3_bsp_agent:handle_request(<<"build/initialized">>, #{}),
  Params = #{ targets => [<<"default">>] },
  Result = rebar3_bsp_agent:handle_request(<<"buildTarget/dependencySources">>, Params),
  {ok, Cwd} = file:get_cwd(),
  ?assertEqual(#{ items => [filename:join([Cwd, "_build", "default", "lib", "meck"])] }, Result),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec sample_app_dir() -> file:filename().
sample_app_dir() ->
  filename:join([code:priv_dir(rebar3_bsp), "sample"]).
