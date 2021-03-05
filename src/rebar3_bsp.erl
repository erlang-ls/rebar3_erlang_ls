-module(rebar3_bsp).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
  {ok, State1} = erl_subgraph_compile_prv:init(State0),
  {ok, State2} = rebar3_bsp_prv:init(State1),
  {ok, State2}.
