-module(rebar3_bsp_ct_hook).

-export([ init/2 ]).

-export([ on_tc_fail/4
        , on_tc_skip/4
        ]).

-define(SERVER, rebar3_bsp_server).

-type id()          :: any().
-type options()     :: #{from := {pid(), reference()}}.
-type suite()       :: atom().
-type testcase()    :: atom() | tuple().
-type state()       :: #{options := options()}.
-type skip_reason() :: {tc_auto_skip | tc_user_skip, any()}.
-type fail_reason() :: any().
-type reason()      :: skip_reason() | fail_reason().

-spec init(id(), options()) -> {ok, state()}.
init(_Id, Opts) ->
  {ok, #{options => Opts}}.

-spec on_tc_fail(suite(), testcase(), fail_reason(), state()) -> state().
on_tc_fail(_Suite, _TestCase, Reason, State) ->
  send_result(Reason, State),
  State.

-spec on_tc_skip(suite(), testcase(), skip_reason(), state()) -> state().
on_tc_skip(_Suite, _TestCase, {_ReasonType, Reason}, State) ->
  send_result(Reason, State),
  State.

-spec send_result(reason(), state()) -> ok.
send_result(Reason, #{options := #{from := From}}) ->
  ?SERVER ! {result, From, Reason},
  ok.
