-module(rebar3_bsp_test).

-export([ sample_app_dir/0
        , init_sample_app_testcase/2
        , end_sample_app_testcase/2
        ]).



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

