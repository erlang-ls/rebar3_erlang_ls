-module(rebar3_bsp_client_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ build_initialize/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: ct_suite:ct_config().

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  %% Required for the application environment to be loaded
  application:load(rebar3_bsp),
  ok = rebar3_bsp_util:clean_sample_app_dir(),
  rebar3_bsp_connection:generate(rebar3_bsp_util:sample_app_dir()),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  {ok, _Pid} = rebar3_bsp_client:start_link({root, rebar3_bsp_util:sample_app_dir()}),
  Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  ok = rebar3_bsp_client:stop(),
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
  RequestId = rebar3_bsp_client:send_request('build/initialize', #{}),
  {ok, Result} = rebar3_bsp_client:receive_response(RequestId, 60 * 1000),
  ?assertMatch(#{ bspVersion := <<"2.0.0">>
                , displayName := <<"rebar3_bsp">>
                , version := <<"0.1.0">>
                , capabilities := #{ canReload := true
                                   , dependencySourcesProvider := true
                                   , compileProvider := #{ languageIds := [<<"erlang">>] }
                                   , testProvider := #{ languageIds := [<<"erlang">>] }
                                   }
                }, Result),
  ok.

