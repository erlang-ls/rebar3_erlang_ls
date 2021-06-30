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
        , workspace_reload/1
        , buildtarget_sources/1
        , buildtarget_dependencysources/1
        , buildtarget_compile/1
        , buildtarget_test/1
        , rebar3_run/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("rebar3_bsp.hrl").

%%==============================================================================
%% Definitions
%%==============================================================================
-define(SAMPLE_APP_DIR, rebar3_bsp_util:sample_app_dir()).
-define(assertMapKeyEqual(Expected, Key, Map), ?assertEqual(Expected, maps:get(Key, Map))).

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
  ok = rebar3_bsp_util:clean_sample_app_dir(),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  SampleDir = rebar3_bsp_util:sample_app_dir(),
  {ok, Cwd, SampleDir} = rebar3_bsp_util:cd(SampleDir),
  State = rebar3:init_config(),
  {ok, _AgentPid} = rebar3_bsp_agent:start_link(State),
  [{cwd, Cwd} | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  OriginalCwd = proplists:get_value(cwd, Config),
  ok = rebar3_bsp_agent:stop(),
  {ok, _, OriginalCwd} = rebar3_bsp_util:cd(OriginalCwd),
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
  {ok, Result} = rebar3_bsp_agent:request('build/initialize', client_caps()),
  ?assertMapKeyEqual(rebar3_bsp_connection:version(rebar3_bsp), version, Result),
  ?assertMapKeyEqual(?BSP_VSN, bspVersion, Result),
  ?assertMapKeyEqual(<<"rebar3_bsp">>, displayName, Result),
  #{ capabilities := Capabilities } = Result,
  ErlangProvider = #{ languageIds => [<<"erlang">>] },
  ?assertMapKeyEqual(ErlangProvider, compileProvider, Capabilities),
  ?assertMapKeyEqual(ErlangProvider, testProvider, Capabilities),
  ?assertMapKeyEqual(true, canReload, Capabilities),
  ?assertMapKeyEqual(true, dependencySourcesProvider, Capabilities),
  ok.

-spec build_initialized(config()) -> ok.
build_initialized(_Config) ->
  {ok, _} = rebar3_bsp_agent:request('build/initialize', client_caps()),
  ok = rebar3_bsp_agent:notify('build/initialized', #{}),
  ok.

-spec workspace_buildtargets(config()) -> ok.
workspace_buildtargets(_Config) ->
  initialize_agent(),
  {ok, Result} = rebar3_bsp_agent:request('workspace/buildTargets', #{}),
  ?assertMatch(#{ targets := [#{ id := #{ uri := <<"profile:default">> } }] }, Result),
  ok.

-spec workspace_reload(config()) -> ok.
workspace_reload(_Config) ->
  initialize_agent(),
  {ok, null} = rebar3_bsp_agent:request('workspace/reload', null),
  ok.

-spec buildtarget_sources(config()) -> ok.
buildtarget_sources(_Config) ->
  initialize_agent(),
  {ok, Result} = rebar3_bsp_agent:request('buildTarget/sources', targets(["default"])),
  #{ items := [Item] } = Result,
  #{ sources := [Source] } = Item,
  ?assertEqual(?SOURCE_ITEM_KIND_DIR, maps:get(kind, Source)),
  ?assertEqual(false, maps:get(generated, Source)),
  ?assertEqual(rebar3_bsp_uri:dir(?SAMPLE_APP_DIR), maps:get(uri, Source)),
  ok.

-spec buildtarget_dependencysources(config()) -> ok.
buildtarget_dependencysources(_Config) ->
  initialize_agent(),
  {ok, Result} = rebar3_bsp_agent:request('buildTarget/dependencySources', targets(["default"])),
  #{ items := [#{ target := #{ uri := <<"profile:default">> }, sources := Sources }] } = Result,
  [ResultMeckDir] = Sources,
  ?assertEqual(rebar3_bsp_uri:dir(sample_app_build_dir("default/lib/meck")), ResultMeckDir),
  ok.

-spec buildtarget_compile(config()) -> ok.
buildtarget_compile(_Config) ->
  initialize_agent(),
  {ok, Result} = rebar3_bsp_agent:request('buildTarget/compile', targets(["default", "test"])),
  ?assertEqual(#{ statusCode => 0 }, Result),
  ?assert(filelib:is_dir(sample_app_build_dir("test/lib/meck"))),
  ?assert(filelib:is_dir(sample_app_build_dir("default/lib/sample"))),
  ok.

-spec buildtarget_test(config()) -> ok.
buildtarget_test(_Config) ->
  initialize_agent(),
  {ok, Result} = rebar3_bsp_agent:request('buildTarget/test', targets(["default"])),
  ?assertMatch(#{ statusCode := 0 }, Result),
  ok.

-spec rebar3_run(config()) -> ok.
rebar3_run(_Config) ->
  initialize_agent(),
  ?assertEqual({ok, #{}}, rebar3_bsp_agent:request('rebar3/run', #{ args => ["version"] })),
  ?assertMatch({error, #{}}, rebar3_bsp_agent:request('rebar3/run', #{ args => ["bogus"] })),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec targets([unicode:chardata()]) -> #{ targets := [#{ uri := binary()}] }.
targets(Targets) ->
  #{ targets => [ target(T, []) || T <- Targets] }.

-spec target(unicode:chardata(), [{unicode:chardata(), unicode:chardata() | true}]) -> #{ uri := binary() }.
target(Profile, Params) ->
  Uri = rebar3_bsp_uri:profile(Profile, Params),
  #{ uri => Uri }.

-spec sample_app_build_dir(string()) -> string().
sample_app_build_dir(Dir) ->
  filename:join([?SAMPLE_APP_DIR, "_build", Dir]).

-spec initialize_agent() -> ok.
initialize_agent() ->
  {ok, _Result} = rebar3_bsp_agent:request('build/initialize', client_caps()),
  ok = rebar3_bsp_agent:notify('build/initialized', #{}),
  ok.

-spec client_caps() -> map().
client_caps() ->
  #{ capabilities => #{ languageIds => [<<"erlang">>] } }.
