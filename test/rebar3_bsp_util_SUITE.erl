-module(rebar3_bsp_util_SUITE).

-export([all/0]).

-export([ to_binary/1
        , to_string/1
        , map_fread/1
        , lists_intersection/1
        , lists_union/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-type config() :: [{atom(), any()}].

all() ->
   [ to_binary
   , to_string
   , map_fread
   , lists_intersection
   , lists_union
   ].

-spec to_binary(config()) -> ok.
to_binary(_Config) ->
  ?assertEqual(<<"ok">>, rebar3_bsp_util:to_binary(ok)),
  ?assertEqual(<<"ok">>, rebar3_bsp_util:to_binary("ok")),
  ?assertEqual(<<"ok">>, rebar3_bsp_util:to_binary(<<"ok">>)),
  ?assertError(badarg, rebar3_bsp_util:to_binary(erlang:make_ref())),
  ok.

-spec to_string(config()) -> ok.
to_string(_Config) ->
  ?assertEqual("ok", rebar3_bsp_util:to_string(ok)),
  ?assertEqual("ok", rebar3_bsp_util:to_string("ok")),
  ?assertEqual("ok", rebar3_bsp_util:to_string(<<"ok">>)),
  ?assertError(badarg, rebar3_bsp_util:to_string(erlang:make_ref())),
  ok.

-spec map_fread(config()) -> ok.
map_fread(_Config) ->
  ?assertEqual({ok, [1024], ""}, rebar3_bsp_util:map_fread(length, #{ length => <<"1024">> }, "~u")),
  ?assertEqual({ok, [1024], "abba"}, rebar3_bsp_util:map_fread(length, #{ length => <<"1024abba">> }, "~u")),
  ?assertEqual({error, {badkey, length}}, rebar3_bsp_util:map_fread(length, #{}, foobarbaz)),
  ?assertEqual({error, {fread, {more, "~u", 0, ""}}}, rebar3_bsp_util:map_fread(length, #{ length => "" }, "~u")),
  ok.

-spec lists_intersection(config()) -> ok.
lists_intersection(_Config) ->
  ?assertEqual([a, b], rebar3_bsp_util:lists_intersection([[b, a, b, a]])),
  ?assertEqual([a, b], rebar3_bsp_util:lists_intersection([[b, a, b, a], [c, d, b, b, a, b]])),
  ok.

-spec lists_union(config()) -> ok.
lists_union(_Config) ->
  ?assertEqual([a,b,c], rebar3_bsp_util:lists_union([[a, a, a, c, b, c]])),
  ?assertEqual([a,b,c], rebar3_bsp_util:lists_union([[a, c, c], [b, a]])),
  ok.

