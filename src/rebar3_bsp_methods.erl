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
        Method(ParamType, rebar3_bsp_server:state()) ->
               {response, ResultType, rebar3_bsp_server:state()}).

-define(NOTIFICATION_SPEC(Method, ParamType),
        Method(ParamType, rebar3_bsp_server:state()) ->
               {noresponse, rebar3_bsp_server:state()}).

-spec ?REQUEST_SPEC('build/initialize', initializeBuildParams(), initializeBuildResult()).
'build/initialize'(_Params, ServerState) ->
  Result = #{ displayName => <<"rebar3_bsp">>
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
    ExitCode = case IsShutdown of
        true ->
            0;
        false ->
            1
    end,
    erlang:halt(ExitCode),
    {noresponse, ServerState}.

-spec ?REQUEST_SPEC('workspace/buildTargets', workspaceBuildTargetsParams(), workspaceBuildTargetsResult()).
'workspace/buildTargets'(_Params, #{rebar3_state := R3State} = ServerState) ->
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
    {response, #{targets => BuildTargets}, ServerState}.

-spec ?REQUEST_SPEC('workspace/reload', null, null).
'workspace/reload'(null, ServerState) ->
    %% TODO
    {reply, null, ServerState}.

-spec ?REQUEST_SPEC('buildTarget/sources', buildTargetSourcesParams(), buildTargetSourcesResult()).
'buildTarget/sources'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
    Items = items(rebar_state:project_apps(R3State), Targets),
    {response, #{items => Items}, ServerState}.

-spec ?REQUEST_SPEC('buildTarget/dependencySources', dependencySourcesParams(), dependencySourcesResult()).
'buildTarget/dependencySources'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
    Items = items(rebar_state:all_deps(R3State), Targets, R3State),
    {response, #{items => Items}, ServerState}.

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
    
