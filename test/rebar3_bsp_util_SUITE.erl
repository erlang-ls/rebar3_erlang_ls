-module(rebar3_bsp_util_SUITE).
-behaviour(ct_suite).

-export([all/0]).

%% TODO
%% -export([ client_request/1
%%         , client_notify/1
%%         , initialize_server/1
%%         , bring_up_local_client_server/1
%%         , tear_down_local_client_server/1
%%         , maybe_stop/1
%%         , new_rebar_state_from_file/1
%%         ]).

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
   %% [ client_request
   %% , client_notify
   %% , initialize_server
   %% , bring_up_local_client_server
   %% , tear_down_local_client_server
   %% , maybe_stop
   %% , new_rebar_state_from_file
   %% ] ++
   [ to_binary
   , to_string
   , map_fread
   , lists_intersection
   , lists_union
   ].

%% -spec client_request(config()) -> ok.
%% client_request(_Config) ->
%%   ok.

%% -spec client_notify(config()) -> ok.
%% client_notify(_Config) ->
%%   ok.

%% -spec initialize_server(config()) -> ok.
%% initialize_server(_Config) ->
%%   ok.

%% -spec bring_up_local_client_server(config()) -> ok.
%% bring_up_local_client_server(_Config) ->
%%   ok.

%% -spec tear_down_local_client_server(config()) -> ok.
%% tear_down_local_client_server(_Config) ->
%%   ok.

%% -spec maybe_stop(config()) -> ok.
%% maybe_stop(_Config) ->
%%   ok.

%% -spec new_rebar_state_from_file(config()) -> ok.
%% new_rebar_state_from_file(_Config) ->
%%   ok.

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

