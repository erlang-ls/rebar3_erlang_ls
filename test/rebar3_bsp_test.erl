-module(rebar3_bsp_test).

-export([ sample_app_dir/0
        , init_sample_app_testcase/2
        , end_sample_app_testcase/2
        , client_request/2
        , client_request/3
        , client_notify/2
        , initialize_server/0]).

-define(TIMEOUT, 500).

-spec sample_app_dir() -> file:name().
sample_app_dir() ->
  PrivDir = rebar_file_utils:resolve_link(code:priv_dir(rebar3_bsp)),
  filename:join([PrivDir, "sample"]).

-spec init_sample_app_testcase(atom(), list()) -> list().
init_sample_app_testcase(_TestCase, Config) ->
  {ok, Cwd} = file:get_cwd(),
  Root = sample_app_dir(),
  ok = file:set_cwd(Root),
  [{sample_app_dir, Root}, {cwd, Cwd} | Config].

-spec end_sample_app_testcase(atom(), list()) -> ok.
end_sample_app_testcase(_TestCase, Config) ->
  Cwd = proplists:get_value(cwd, Config),
  ok = file:set_cwd(Cwd),
  ok.

-spec client_request(binary() | atom(), map()) -> any().
client_request(Method, Params) ->
  client_request(Method, Params, ?TIMEOUT).

-spec client_request(binary() | atom(), map(), timeout()) -> any().
client_request(Method, Params, Timeout) ->
  RequestId = rebar3_bsp_client:send_request(Method, Params),
  rebar3_bsp_client:receive_response(RequestId, Timeout).

-spec client_notify(binary() | atom(), map()) -> ok.
client_notify(Method, Params) ->
  ok = rebar3_bsp_client:send_notification(Method, Params),
  ok.

-spec initialize_server() -> ok.
initialize_server() ->
  {ok, _Result} = client_request('build/initialize', #{}),
  ok = client_notify('build/initialized', #{}),
  ok.
