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
        , buildtarget_compile/1
        , buildtarget_sources/1
        , buildtarget_dependencysources/1
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
-define(SAMPLE_APP_DIR, rebar3_bsp_test:sample_app_dir()).

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
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config0) ->
  Config1 = rebar3_bsp_test:init_sample_app_testcase(TestCase, Config0),
  State = rebar3_bsp_util:new_rebar_state_from_file("rebar.config"),
  {ok, LocalConfig} = rebar3_bsp_util:bring_up_local_client_server(State),
  [{local_client_server, LocalConfig} | Config1].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  LocalConfig = proplists:get_value(local_client_server, Config),
  ok = rebar3_bsp_util:tear_down_local_client_server(LocalConfig),
  ok = rebar3_bsp_test:end_sample_app_testcase(TestCase, Config),
  ok.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

sync(Config) ->
  {EchoPort, _ClientPid, _ServerPid} = proplists:get_value(local_client_server, Config),
  ok = rebar3_bsp_echo_port:sync(EchoPort),
  ok.

%%==============================================================================
%% Testcases
%%==============================================================================
-spec build_initialize(config()) -> ok.
build_initialize(_Config) ->
  {ok, Result} = rebar3_bsp_util:client_request('build/initialize', #{}),
  ExpectedVersion = rebar3_bsp_connection:version(rebar3_bsp),
  ?assertMatch(#{ bspVersion := ?BSP_VSN
                , capabilities := #{ compileProvider := [<<"erlang">>]
                                   , testProvider := [<<"erlang">>]
                                   , dependencySourcesProvider := true
                                   }
                , displayName := <<"rebar3_bsp">>
                , version := ExpectedVersion
                }, Result),
  ?assertMatch(#{ is_initialized := false }, server_state()),
  ok.

-spec build_initialized(config()) -> ok.
build_initialized(Config) ->
  {ok, _} = rebar3_bsp_util:client_request('build/initialize', #{}),
  ok = rebar3_bsp_util:client_notify('build/initialized', #{}),
  sync(Config),
  ServerState = server_state(),
  ?assertMatch(#{ is_initialized := true }, ServerState),
  ok.

-spec workspace_buildtargets(config()) -> ok.
workspace_buildtargets(_Config) ->
  rebar3_bsp_util:initialize_server(),
  {ok, Result} = rebar3_bsp_util:client_request('workspace/buildTargets', #{}),
  ?assertMatch(#{ targets := [#{ id := #{ uri := <<"profile:default">> } }] }, Result),
  ok.

-spec workspace_reload(config()) -> ok.
workspace_reload(Config) ->
  rebar3_bsp_util:initialize_server(),
  sync(Config),
  ?assertMatch(#{ is_initialized := true }, server_state()),
  {ok, null} = rebar3_bsp_util:client_request('workspace/reload', null),
  sync(Config),
  ?assertMatch(#{ is_initialized := true }, server_state()),
  ok.

-spec buildtarget_compile(config()) -> ok.
buildtarget_compile(_Config) ->
  rebar3_bsp_util:initialize_server(),
  {ok, Result} = rebar3_bsp_util:client_request('buildTarget/compile'
                                               , targets([<<"profile:default">>, <<"profile:test">>])),
  ?assertEqual(#{ statusCode => 0 }, Result),
  ?assert(filelib:is_dir(sample_app_build_dir("test/lib/meck"))),
  ?assert(filelib:is_dir(sample_app_build_dir("default/lib/sample"))),
  ok.

-spec buildtarget_sources(config()) -> ok.
buildtarget_sources(_Config) ->
  rebar3_bsp_util:initialize_server(),
  {ok, Result} = rebar3_bsp_util:client_request('buildTarget/sources', targets([<<"profile:default">>])),
  #{ items := [Item] } = Result,
  #{ sources := [Source] } = Item,
  ?assertEqual(?SOURCE_ITEM_KIND_DIR, maps:get(kind, Source)),
  ?assertEqual(false, maps:get(generated, Source)),
  ?assertEqual(rebar3_bsp_uri:dir(?SAMPLE_APP_DIR), maps:get(uri, Source)),
  ok.

-spec buildtarget_dependencysources(config()) -> ok.
buildtarget_dependencysources(_Config) ->
  rebar3_bsp_util:initialize_server(),
  {ok, Result} = rebar3_bsp_util:client_request('buildTarget/dependencySources', targets([<<"profile:default">>])),
  #{ items := [#{ target := #{ uri := <<"profile:default">> }, sources := Sources }] } = Result,
  [ResultMeckDir] = Sources,
  ?assertEqual(rebar3_bsp_uri:dir(sample_app_build_dir("default/lib/meck")), ResultMeckDir),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec targets([binary()]) -> #{ targets => [#{ uri := binary()}] }.
targets(Targets) ->
  #{ targets => [#{ uri => T } || T <- Targets] }.

-spec sample_app_build_dir(string()) -> string().
sample_app_build_dir(Dir) ->
  filename:join([?SAMPLE_APP_DIR, "_build", Dir]).

-spec server_state() -> term().
server_state() ->
  sys:get_state(rebar3_bsp_server).

-spec client_state() -> term().
client_state() ->
  sys:get_state(rebar3_bsp_client).

