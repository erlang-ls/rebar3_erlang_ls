-module(rebar3_bsp_uri).

-export([ file/1
        , dir/1
        , profile/1
        , sanitize/1
        , compose/1
        , extract/3
        , normalize/1
        , normalize/2
        , parse/1
        ]).

-type uri_string() :: uri_string:uri_string().
-type uri_map()    :: uri_string:uri_map().

-spec file(file:name_all()) -> binary().
file(Filename) ->
  %% Filenames must never have a trailing slash
  Sanitized = sanitize(Filename),
  compose(#{ path => Sanitized }).

-spec dir(file:name_all()) -> binary().
dir(Dirname) ->
  %% Dirnames must always end with a slash
  Sanitized = sanitize(Dirname),
  compose(#{ path => <<Sanitized/binary, "/">> }).

-spec profile(file:name_all()) -> binary().
profile(Profile) ->
  Sanitized = sanitize(Profile),
  compose(#{ scheme => "profile", path => Sanitized }).

-spec sanitize(file:name_all()) -> binary().
sanitize(Filename) ->
  Flattened = filename:flatten(Filename),
  Rejoined = filename:join(filename:split(Flattened)),
  rebar3_bsp_util:to_binary(Rejoined).

-spec compose(uri_map()) -> binary().
compose(UriMap) ->
  DefaultsMap = #{ scheme => "file" },
  EffectiveMap = maps:merge(DefaultsMap, UriMap),
  Uri = uri_string:recompose(EffectiveMap),
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
  uri_string:normalize(Uri, [return_map]).

