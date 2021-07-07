-module(rebar3_bsp_uri_SUITE).
-export([all/0]).

-export([ file/1
        , dir/1
        , profile/1
        , sanitize/1
        , compose/1
        , extract/1
        , normalize/1
        , parse/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Definitions
%%==============================================================================
-define(M, rebar3_bsp_uri).

%%==============================================================================
%% CT Callbacks
%%==============================================================================
all() ->
  [ file, dir, profile, sanitize, compose, extract, normalize, parse ].

%%==============================================================================
%% Test Cases
%%==============================================================================
file(_Config) ->
  ?assertEqual(<<"file:">>, ?M:file("")),
  ?assertEqual(<<"file:foo">>, ?M:file([fo, "o"])),
  ?assertEqual(<<"file:/foo">>, ?M:file(<<"/foo/">>)),
  ok.

dir(_Config) ->
  ?assertEqual(<<"file:foo/">>, ?M:dir("foo")),
  ?assertEqual(<<"file:/">>, ?M:dir("/")),
  ?assertEqual(<<"file:">>, ?M:dir("")),
  ok.

profile(_Config) ->
  ?assertEqual(<<"profile:default">>, ?M:profile(default)),
  ?assertEqual(<<"profile:default">>, ?M:profile(default, [])),
  ?assertEqual(<<"profile:default?--foo=bar">>, ?M:profile("default", [{<<"--foo">>, "bar"}])),
  ok.

sanitize(_Config) ->
  ?assertEqual(<<"/foo/bar/baz">>, ?M:sanitize("///foo/bar//baz//")),
  ?assertEqual(<<"foo/bar/../baz">>, ?M:sanitize(<<"foo/bar/../baz//">>)),
  ?assertEqual(<<"">>, ?M:sanitize([])),
  ?assertEqual(<<"/">>, ?M:sanitize('/')),
  ok.

compose(_Config) ->
  ?assertEqual(<<"http:foo">>, ?M:compose(#{ scheme => "http", path => "foo" })),
  ok.

extract(_Config) ->
  ?assertEqual(<<"default">>, ?M:extract(path, <<"profile:default">>, #{ scheme => "profile" })),
  FileScheme = #{ scheme => <<"file">> },
  ?assertError({badmatch, FileScheme}, ?M:extract(path, <<"file:default">>, #{ scheme => "profile" })),
  ?assertError({badmatch, FileScheme}, ?M:extract(path, "file:default", #{ scheme => "profile" })),
  ok.

normalize(_Config) ->
  ?assertEqual(<<"http://localhost/">>, ?M:normalize("http://localhost")),
  ?assertEqual(#{ scheme => <<"http">>, host => <<"localhost">>, path => <<"/bar///">> },
               ?M:normalize("http://localhost/foo/../bar///", [return_map])),
  ok.

parse(_Config) ->
  ?assertEqual(#{ scheme => <<"http">>, host => <<"localhost">>, path => <<"/">> },
               ?M:parse(#{ scheme => "http", host => "localhost", path => <<"foo/..">> })),
  ok.

