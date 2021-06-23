-module(rebar3_bsp_methods).

%% request methods
-export([ 'build/initialize'/2
        , 'build/shutdown'/2
        , 'workspace/buildTargets'/2
        , 'workspace/reload'/2
        , 'buildTarget/compile'/2
        , 'buildTarget/sources'/2
        , 'buildTarget/dependencySources'/2
        ]).

%% notification methods
-export([ 'build/initialized'/2
        , 'build/exit'/2
        ]).

-include("rebar3_bsp.hrl").

-define(REQUEST_SPEC(Method, ParamType, ResultType),
        Method(ParamType, state()) ->
           {response, ResultType, state()}).

-define(NOTIFICATION_SPEC(Method, ParamType),
        Method(ParamType, state()) ->
           {noresponse, state()} | {exit, integer(), state()}).

-type state() :: rebar3_bsp_server:state().

-spec ?REQUEST_SPEC('build/initialize', initializeBuildParams(), initializeBuildResult()).
'build/initialize'(_Params, ServerState) ->
  Result = #{ displayName => rebar3_bsp
            , version => rebar3_bsp_connection:version(?BSP_APPLICATION)
            , bspVersion => ?BSP_VSN
            , capabilities => #{}
            },
  {response, Result, ServerState}.

-spec ?NOTIFICATION_SPEC('build/initialized', initializedBuildParams()).
'build/initialized'(#{}, #{rebar3_state := R3State} = ServerState) ->
  {ok, NewR3State} = rebar3:run(R3State, ["compile"]),
  {noresponse, ServerState#{is_initialized => true, rebar3_state => NewR3State}}.

-spec ?REQUEST_SPEC('build/shutdown', null, null).
'build/shutdown'(null, ServerState) ->
  {response, null, ServerState#{is_shutdown => true}}.

-spec ?NOTIFICATION_SPEC('build/exit', null).
'build/exit'(null, #{is_shutdown := IsShutdown} = ServerState) ->
  case IsShutdown of
    true ->
      {exit, 0, ServerState};
    false ->
      {exit, 1, ServerState}
  end.

-spec ?REQUEST_SPEC('workspace/buildTargets', workspaceBuildTargetsParams(), workspaceBuildTargetsResult()).
'workspace/buildTargets'(_Params, #{rebar3_state := R3State} = ServerState) ->
  BuildTargets = [#{ id => #{ uri => uri(profile, Profile) }
                   , tags => []
                   , capabilities => #{ canCompile => true
                                      , canTest => false
                                      , canRun => false
                                      , canDebug => false
                                      }
                   , languageIds => [erlang]
                   , dependencies => []
                   }
                  || Profile <- rebar_state:current_profiles(R3State)],
  {response, #{targets => BuildTargets}, ServerState}.

-spec ?REQUEST_SPEC('workspace/reload', null, null).
'workspace/reload'(null, ServerState) ->
  %% TODO
  {response, null, ServerState}.

-spec ?REQUEST_SPEC('buildTarget/sources', buildTargetSourcesParams(), buildTargetSourcesResult()).
'buildTarget/sources'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
  Items = items(project_apps, Targets, R3State),
  {response, #{items => Items}, ServerState}.

-spec ?REQUEST_SPEC('buildTarget/dependencySources', dependencySourcesParams(), dependencySourcesResult()).
'buildTarget/dependencySources'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
  Items = items(all_deps, Targets, R3State),
  {response, #{items => Items}, ServerState}.

-spec ?REQUEST_SPEC('buildTarget/compile', compileParams(), compileResult()).
'buildTarget/compile'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
  {ok, NewR3State} = rebar3:run(R3State, ["compile"]),
  {response, #{ statusCode => 0 }, ServerState#{rebar3_state => NewR3State}}.

%% Internal Functions

-spec items(atom(), [buildTargetIdentifier()], rebar3_state:t()) -> [sourcesItem()].
items(Items, Targets, R3State) ->
  Applications = rebar_state:Items(R3State),
  TargetProfiles = [binary_to_atom(uri_extract(path, Uri, #{scheme => <<"profile">>}))
                    || #{ uri := Uri } <- Targets],
  TargetsAndApps = [{Target, A} || A <- Applications,
                                   {Target, Profile} <- lists:zip(Targets, TargetProfiles),
                                   lists:member(Profile, rebar_app_info:profiles(A))],
  TargetMap = lists:foldl(
                fun({Target, App}, Acc) ->
                    {Sources, Roots} = maps:get(Acc, Target, {[], []}),
                    AppDir = ensure_binary(rebar_app_info:dir(App)),
                    Source = #{ uri => AppDir
                              , kind => ?SOURCE_ITEM_KIND_DIR
                              , generated => false
                              },
                    Root = ensure_binary(rebar_state:dir(R3State)),
                    Acc#{ Target => {[Source|Sources], [Root|Roots]} }
                end, #{}, TargetsAndApps),
  [ #{ target => Target, sources => Sources, roots => Roots}
    || {Target, {Sources, Roots}} <- maps:to_list(TargetMap)].

-spec uri(atom() | binary(), atom() | binary()) -> binary().
uri(Scheme, Path) ->
  SchemeBin = ensure_binary(Scheme),
  PathBin = ensure_binary(Path),
  uri_string:recompose(#{scheme => SchemeBin, path => PathBin}).

-spec ensure_binary(atom() | binary() | string()) -> binary().
ensure_binary(A) when is_atom(A) ->
  atom_to_binary(A);
ensure_binary(S) when is_list(S) ->
  list_to_binary(S);
ensure_binary(B) when is_binary(B) ->
  B.

-spec uri_extract(atom(), uri_string:uri_string(), uri_string:uri_map()) -> binary().
uri_extract(Key, Uri, Checks) ->
  Parsed = uri_string:parse(Uri),
  Checks = maps:with(maps:keys(Checks), Parsed),
  maps:get(Key, Parsed).

