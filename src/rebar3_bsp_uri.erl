-module(rebar3_bsp_uri).

-export([ file/1
        , dir/1
        , profile/1, profile/2
        , sanitize/1
        , compose/1
        , extract/3
        , normalize/1
        , normalize/2
        , parse/1
        ]).

-type uri_string() :: uri_string:uri_string().
-type uri_map()    :: uri_string:uri_map().

-define(PROFILE_SCHEME, <<"profile">>).
-define(FILE_SCHEME, <<"file">>).

-spec file(file:name_all()) -> binary().
file(Filename) ->
  %% Filenames must never have a trailing slash
  Sanitized = sanitize(Filename),
  compose(#{ scheme => ?FILE_SCHEME, path => Sanitized }).

-spec dir(file:name_all()) -> binary().
dir(Dirname) ->
  %% Dirnames must always end with a slash
  Sanitized = case sanitize(Dirname) of
                <<"">> ->
                  <<"">>;
                <<"/">> ->
                  <<"/">>;
                Else ->
                  <<Else/binary, "/">>
              end,
  compose(#{ scheme => ?FILE_SCHEME, path => Sanitized }).

-spec profile(atom() | unicode:chardata()) -> binary().
profile(Profile) ->
  profile(Profile, []).

-spec profile(atom() | unicode:chardata(), [{unicode:chardata(), unicode:chardata() | true}]) -> binary().
profile(Profile, Params) ->
  BaseMap = #{ scheme => ?PROFILE_SCHEME, path => rebar3_bsp_util:to_binary(Profile) },
  UriMap = case Params of
             [] ->
               BaseMap;
             Params ->
               BaseMap#{ query => uri_string:compose_query(Params) }
           end,
  compose(UriMap).

-spec sanitize(file:name_all()) -> binary().
sanitize(Filename) ->
  Flattened = filename:flatten(Filename),
  Parts = filename:split(Flattened),
  Rejoined = case Parts of
               [] ->
                 "";
               Parts ->
                 filename:join(Parts)
             end,
  rebar3_bsp_util:to_binary(Rejoined).

-spec compose(uri_map()) -> binary().
compose(UriMap) ->
  Uri = uri_string:recompose(UriMap),
  rebar3_bsp_util:to_binary(Uri).

-spec extract(atom(), uri_string() | uri_map(), uri_map()) -> binary().
extract(Key, Uri, Checks) ->
  NormalizedUri = normalize(Uri, [return_map]),
  %% Checks might not be a full uri_map, so a regular normalize call might barf
  %% - just ensure the values are binaries
  NormalizedChecks = maps:map(fun(_K, V) -> rebar3_bsp_util:to_binary(V) end, Checks),
  %% Verify that the requested checks match
  NormalizedChecks = maps:with(maps:keys(NormalizedChecks), NormalizedUri),
  %% Uri checks out, extract the requested part
  rebar3_bsp_util:to_binary(maps:get(Key, NormalizedUri)).

-spec normalize(uri_string() | uri_map()) -> uri_string().
normalize(Uri) ->
  normalize(Uri, []).

-spec normalize(uri_string() | uri_map(), [] | [return_map]) -> uri_string() | uri_map().
normalize(Uri, Opts) ->
  NormalizedUri = uri_string:normalize(Uri),
  BinaryUri = rebar3_bsp_util:to_binary(NormalizedUri),
  case Opts of
    [] ->
      BinaryUri;
    [return_map] ->
      uri_string:parse(BinaryUri)
  end.

-spec parse(uri_string()) -> uri_map().
parse(Uri) ->
  normalize(Uri, [return_map]).

