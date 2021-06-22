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
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  %% Required for the application environment to be loaded
  application:load(rebar3_bsp),
  rebar3_bsp_connection:generate(rebar3_bsp_test:sample_app_dir()),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
  Config1 = rebar3_bsp_test:init_sample_app_testcase(TestCase, Config),
  {ok, _Pid} = rebar3_bsp_client:start_link({root, rebar3_bsp_test:sample_app_dir()}),
  Config1.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  ok = rebar3_bsp_test:end_sample_app_testcase(TestCase, Config),
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
  {ok, Result} = rebar3_bsp_test:client_request('build/initialize', #{}, 30 * 1000),
  Expected = #{ bspVersion => <<"2.0.0">>
              , capabilities => #{}
              , displayName => <<"rebar3_bsp">>
              , version => rebar3_bsp_connection:version(rebar3_bsp)
              },
  ?assertEqual(Expected, Result),
  ok.

