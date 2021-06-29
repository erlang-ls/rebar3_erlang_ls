-module(rebar3_bsp_test).

-export([ sample_app_dir/0
        , init_sample_app_testcase/2
        , end_sample_app_testcase/2
        ]).

-spec sample_app_dir() -> file:name().
sample_app_dir() ->
  PrivDir = code:priv_dir(rebar3_bsp),
  ResolvedPrivDir = rebar_file_utils:resolve_link(PrivDir),
  Result = filename:join([ResolvedPrivDir, "sample"]),
  Result.

-spec init_sample_app_testcase(atom(), list()) -> list().
init_sample_app_testcase(_TestCase, Config) ->
  {ok, Cwd, Root} = rebar3_bsp_util:cd(sample_app_dir()),
  [{sample_app_dir, Root}, {cwd, Cwd} | Config].

-spec end_sample_app_testcase(atom(), list()) -> ok.
end_sample_app_testcase(_TestCase, Config) ->
  Cwd = proplists:get_value(cwd, Config),
  {ok, _PrevDir, Cwd} = rebar3_bsp_util:cd(Cwd),
  ok.

