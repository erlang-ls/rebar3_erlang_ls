-module(rebar3_bsp_methods).

%% request methods
-export([ 'build/initialize'/3
        , 'build/shutdown'/3
        , 'workspace/buildTargets'/3
        , 'workspace/reload'/3
        , 'buildTarget/sources'/3
        , 'buildTarget/dependencySources'/3
        ]).

%% notification methods
-export([ 'build/initialized'/3
        , 'build/exit'/3
        ]).

-include("rebar3_bsp.hrl").

-define(REQUEST_SPEC(Method, ParamType, ResultType),
        Method(ParamType, server_state(), rebar3_state:t()) ->
               {ResultType, server_state(), rebar3_state:t()}).

-define(NOTIFICATION_SPEC(Method, ParamType),
        Method(ParamType, server_state(), rebar3_state:t()) ->
               {ok, server_state(), rebar3_state:t()}).

-spec ?REQUEST_SPEC('build/initialize', initializeBuildParams(), initializeBuildResult()).
'build/initialize'(_Params, ServerState, R3State) ->
  Result = #{ displayName => <<"rebar3_bsp">>
            , version => rebar3_bsp_connection:version(?BSP_APPLICATION)
            , bspVersion => ?BSP_VSN
            , capabilities => #{}
            },
  {Result, ServerState, R3State}.

-spec ?NOTIFICATION_SPEC('build/initialized', initializedBuildParams()).
'build/initialized'(#{}, ServerState, R3State) ->
    {ok, NewR3State} = rebar3:run(R3State, ["compile"]),
    {ok, ServerState#{is_initialized => true}, NewR3State}.

-spec ?REQUEST_SPEC('build/shutdown', null, null).
'build/shutdown'(null, ServerState, R3State) ->
    {null, ServerState#{is_shutdown => true}, R3State}.

-spec ?NOTIFICATION_SPEC('build/exit', null).
'build/exit'(null, ServerState, R3State) ->
    ExitCode = case ServerState of
        #{is_shutdown := true} ->
            0;
        _ ->
            1
    end,
    erlang:halt(ExitCode),
    {ok, ServerState, R3State}.

-spec ?REQUEST_SPEC('workspace/buildTargets', workspaceBuildTargetsParams(), workspaceBuildTargetsResult()).
'workspace/buildTargets'(_Params, ServerState, R3State) ->
    BuildTargets = [#{ id => #{ uri => atom_to_binary(Profile) }
                     , tags => []
                     , capabilities => #{ canCompile => true
                                        , canTest => false
                                        , canRun => false
                                        , canDebug => false
                                        }
                     , languageIds => [<<"erlang">>]
                     , dependencies => []
                     }
                    || Profile <- rebar_state:current_profiles(R3State)],
    {#{targets => BuildTargets}, ServerState, R3State}.

-spec ?REQUEST_SPEC('workspace/reload', null, null).
'workspace/reload'(null, ServerState, R3State) ->
    %% TODO
    {null, ServerState, R3State}.

-spec ?REQUEST_SPEC('buildTarget/sources', buildTargetSourcesParams(), buildTargetSourcesResult()).
'buildTarget/sources'(#{targets := Targets}, ServerState, R3State) ->
    Items = items(rebar_state:project_apps(R3State), Targets, R3State),
    {#{items => Items}, ServerState, R3State}.

-spec ?REQUEST_SPEC('buildTarget/dependencySources', dependencySourcesParams(), dependencySourcesResult()).
'buildTarget/dependencySources'(#{targets := Targets}, ServerState, R3State) ->
    Items = items(rebar_state:all_deps(R3State), Targets, R3State),
    {#{items => Items}, ServerState, R3State}.

%% Internal Functions

-spec items([atom()], [buildTargetIdentifier()], rebar3_state:t()) -> [sourcesItem()]. 
items(Applications, Targets, R3State) ->
    TargetProfiles = [erlang:binary_to_atom(Uri) || #{ uri := Uri } <- Targets],
    TargetsAndApps = [{Target, A} || A <- Applications,
                                     {Target, Profile} <- lists:zip(Targets, TargetProfiles),
                                     lists:member(Profile, rebar_app_info:profiles(A))],
    TargetMap = lists:foldl(
        fun({Target, App}, Acc) ->
            {Sources, Roots} = maps:get(Acc, Target, {[], []}),
            AppDir = rebar_app_info:dir(App),
            Source = #{ uri => erlang:list_to_binary(AppDir)
                      , kind => ?SOURCE_ITEM_KIND_DIR
                      , generated => false
                      },
            Root = rebar_state:dir(R3State),
            Acc#{ Target => {[Source|Sources], [Root, Roots]} }
        end, #{}, TargetsAndApps),
    [ #{ target => Target, sources => Sources, roots => Roots}
      || {Target, {Sources, Roots}} <- maps:to_list(TargetMap)].
    
