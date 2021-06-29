-module(sample_app_SUITE).
-behaviour(ct_suite).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ sample_app/1 ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PING_PONG_SERVER, sample_app_ping_pong).

-type testname() :: ct_suite:ct_testname().
-type config() :: ct_suite:ct_config().
-type app() :: {atom(), string(), string()}.
-type apps() :: [app()].
-type app_error() :: {term(), app()}.
-type app_errors() :: [app_error()].

all() ->
  [ sample_app ].

-spec get_apps() -> {apps(), apps()}.
get_apps() ->
  { application:loaded_applications(), application:which_applications() }.

-spec reset_apps(apps(), apps(), atom(), atom()) -> app_errors().
reset_apps(OldApplications, NewApplications, Operation, IgnoredError) ->
  lists:foldl(fun({A, _, _} = App, Acc) ->
                  case lists:member(App, OldApplications) of
                    true ->
                      Acc;
                    false ->
                      case application:Operation(A) of
                        ok ->
                          Acc;
                        {error, {IgnoredError, A}} ->
                          Acc;
                        {error, Reason} ->
                          [{Reason, App}|Acc]
                      end
                  end
              end, [], NewApplications).

-spec init_per_testcase(testname(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  [{initial_apps, get_apps()} | Config].

-spec end_per_testcase(testname(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  {OldLoaded, OldRunning} = proplists:get_value(initial_apps, Config),
  {NewLoaded, NewRunning} = get_apps(),
  ?assertEqual([], reset_apps(OldRunning, NewRunning, stop, not_started)),
  ?assertEqual([], reset_apps(OldLoaded, NewLoaded, unload, not_loaded)),
  ok.

sample_app(_Config) ->
  {ok, [sample]} = application:ensure_all_started(sample),
  Ref = erlang:make_ref(),
  ?assertEqual(ok, ?PING_PONG_SERVER:ping(Ref)),
  Received = receive
               {Ref, X} ->
                 {Ref, X}
             after 500 ->
                 []
             end,
  ServerPid = erlang:whereis(?PING_PONG_SERVER),
  ?assertEqual({Ref, ServerPid}, Received),
  ok.

