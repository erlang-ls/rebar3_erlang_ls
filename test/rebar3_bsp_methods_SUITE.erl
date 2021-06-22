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
-include("rebar3_bsp.hrl").

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
  SampleAppDir = sample_app_dir(),
  ok = file:set_cwd(SampleAppDir),
  {ok, RebarConfig} = file:consult("rebar.config"),
  State = rebar_state:new(RebarConfig),
  {ok, EchoPort} = rebar3_bsp_echo_port:start_link(),
  {ok, ClientPid} = rebar3_bsp_client:start_link({port, EchoPort}),
  {ok, ServerPid} = rebar3_bsp_server:start_link(#{ rebar3_state => State
                                                  , port => EchoPort
                                                  }),
  ok = rebar3_bsp_echo_port:set_endpoints(EchoPort, {ClientPid, ServerPid}),
  [{sample_app_dir, SampleAppDir}, {cwd, Cwd}, {echo_port, EchoPort}] ++ Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  ok = rebar3_bsp_client:stop(),
  ok = rebar3_bsp_server:stop(),
  EchoPort = proplists:get_value(echo_port, Config),
  ok = rebar3_bsp_echo_port:stop(EchoPort),
  Cwd = proplists:get_value(cwd, Config),
  ok = file:set_cwd(Cwd),
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
  {ok, Result} = client_request('build/initialize', #{}),
  ?assertEqual( #{ bspVersion => ?BSP_VSN
                 , capabilities => #{}
                 , displayName => <<"rebar3_bsp">>
                 , version => rebar3_bsp_connection:version(rebar3_bsp)
                 }, Result),
  ok.

-spec build_initialized(config()) -> ok.
build_initialized(_Config) ->
  {ok, _Result} = client_request('build/initialize', #{}),
  Result = client_notify('build/initialized', #{}),
  ?assertEqual(ok, Result),
  ok.

-spec workspace_buildtargets(config()) -> ok.
workspace_buildtargets(_Config) ->
  {ok, Result} = client_request('workspace/buildTargets', #{}),
  ?assertMatch(#{ targets := [#{ id := #{ uri := <<"default">> } }] }, Result),
  ok.

-spec buildtarget_compile(config()) -> ok.
buildtarget_compile(_Config) ->
  {ok, Result} = client_request('buildTarget/compile', #{ targets => []}),
  ?assertEqual(#{ statusCode => 0 }, Result),
  ok.

-spec buildtarget_sources(config()) -> ok.
buildtarget_sources(_Config) ->
  initialize_server(),
  {ok, Result} = client_request('buildTarget/sources', targets([<<"default">>])),
  #{ items := [Item] } = Result,
  #{ roots := [Root] } = Item,
  ?assertEqual(sample_app_dir(), Root),
  ok.

-spec buildtarget_dependencysources(config()) -> ok.
buildtarget_dependencysources(_Config) ->
  initialize_server(),
  {ok, Result} = client_request('buildTarget/dependencySources', targets([<<"default">>])),
  ExpectedMeckDir = filename:join([sample_app_dir(), "_build", "default", "lib", "meck"]),
  #{ items := [#{ sources := [#{ generated := false
                               , kind := ?SOURCE_ITEM_KIND_DIR
                               , uri := ResultMeckDir }] }] } = Result,
  ?assertEqual(ExpectedMeckDir, ResultMeckDir),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec client_request(binary() | atom(), map()) -> any().
client_request(Method, Params) ->
  RequestId = rebar3_bsp_client:send_request(Method, Params),
  rebar3_bsp_client:receive_response(RequestId, 500).

-spec client_notify(binary() | atom(), map()) -> ok.
client_notify(Method, Params) ->
  ok = rebar3_bsp_client:send_notification(Method, Params),
  ok.

-spec sample_app_dir() -> binary().
sample_app_dir() ->
  PrivDir = rebar_file_utils:resolve_link(code:priv_dir(rebar3_bsp)),
  Dir = filename:join([PrivDir, "sample"]),
  list_to_binary(Dir).

-spec targets([binary()]) -> #{ targets => [#{ uri := binary()}] }.
targets(Targets) ->
  #{ targets => [#{ uri => T } || T <- Targets] }.

-spec initialize_server() -> ok.
initialize_server() ->
  {ok, _Result} = client_request('build/initialize', #{}),
  ok = client_notify('build/initialized', #{}),
  ok.

