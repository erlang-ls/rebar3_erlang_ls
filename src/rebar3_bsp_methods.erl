-module(rebar3_bsp_methods).

%% request methods
-export([ 'build/initialize'/2
        , 'build/shutdown'/2
        , 'workspace/buildTargets'/2
        , 'workspace/reload'/2
        , 'buildTarget/sources'/2
        , 'buildTarget/dependencySources'/2
        , 'buildTarget/compile'/2
        , 'buildTarget/test'/2
        , 'rebar3/run'/2
        ]).

%% notification methods
-export([ 'build/initialized'/2
        , 'build/exit'/2
        ]).

-include("rebar3_bsp.hrl").

-define(REQUEST_SPEC(Method, ParamType, ResultType),
        Method(ParamType, state()) ->
           {response, ResultType, state()} | {error, responseError(), state()}).

-define(NOTIFICATION_SPEC(Method, ParamType),
        Method(ParamType, state()) ->
           {noresponse, state()} | {exit, integer(), state()}).

-type state() :: rebar3_bsp_agent:state().

-spec ?REQUEST_SPEC('build/initialize', initializeBuildParams(), initializeBuildResult()).
'build/initialize'(#{ capabilities := #{ languageIds := ClientLanguages  } } = _Params, ServerState) ->
  %% * The server must never respond with build targets for other
  %% * languages than those that appear in this list. */
  ServerLanguages = [<<"erlang">>],
  MutualLanguages = rebar3_bsp_util:lists_intersection([ClientLanguages, ServerLanguages]),
  SupportedProvider = #{ languageIds => MutualLanguages },
  UnsupportedProvider = #{ languageIds => [] },
  Capabilities = #{ compileProvider => SupportedProvider % Advertized iff client supports
                  , testProvider => SupportedProvider    % Advertized iff client supports
                  , runProvider => UnsupportedProvider   % Advertized iff client supports - TODO
                  , debugProvider => UnsupportedProvider % Advertized iff client supports - TODO
                  , inverseSourcesProvider => false      % Always advertized - TODO
                  , dependencySourcesProvider => true    % Always advertized
                  , dependencyModulesProvider => false   % Always advertized - TODO?
                  , resourcesProvider => false           % Always advertized - TODO?
                  , canReload => true                    % Always advertized
                  , buildTargetChangedProvider => false  % Always advertized - TODO
                  },
  Result = #{ displayName => <<"rebar3_bsp">>
            , version => rebar3_bsp_connection:version(?BSP_APPLICATION)
            , bspVersion => ?BSP_VSN
            , capabilities => Capabilities
            },
  {response, Result, ServerState}.

-spec ?NOTIFICATION_SPEC('build/initialized', initializedBuildParams()).
'build/initialized'(_Params, #{rebar3_state := R3State} = ServerState) ->
  {ok, NewR3State} = run(["lock"], R3State),
  {noresponse, ServerState#{is_initialized => true, rebar3_state => NewR3State}}.

-spec ?REQUEST_SPEC('build/shutdown', null, null).
'build/shutdown'(_Params, ServerState) ->
  {response, null, ServerState#{is_shutdown => true}}.

-spec ?NOTIFICATION_SPEC('build/exit', null).
'build/exit'(_Params, #{is_shutdown := IsShutdown} = ServerState) ->
  case IsShutdown of
    true ->
      {exit, 0, ServerState};
    false ->
      {exit, 1, ServerState}
  end.

-spec ?REQUEST_SPEC('workspace/buildTargets', workspaceBuildTargetsParams(), workspaceBuildTargetsResult()).
'workspace/buildTargets'(_Params, #{rebar3_state := R3State} = ServerState) ->
  BuildTargets = [#{ id => #{ uri => rebar3_bsp_uri:profile(Profile) }
                   , tags => [<<"rebar3_profile">>]
                   , capabilities => #{ canCompile => true
                                      , canTest => true
                                      , canRun => false
                                      , canDebug => false
                                      }
                   , languageIds => [<<"erlang">>]
                   , dependencies => []
                   } || Profile <- rebar_state:current_profiles(R3State)],
  {response, #{targets => BuildTargets}, ServerState}.

-spec ?REQUEST_SPEC('workspace/reload', null, null).
'workspace/reload'(_Params, #{rebar3_state := R3State} = ServerState) ->
  BaseDir = rebar_state:dir(R3State),
  Result = case rebar3_bsp_util:cd(BaseDir) of
             {ok, _OldDir, BaseDir} ->
               run(["lock"], R3State);
             Error ->
               Error
           end,
  case Result of
    {ok, NewR3State} ->
      {response, null, ServerState#{ rebar3_state => NewR3State }};
    {error, Reason} ->
      Message = io_lib:format("~p", [Reason]),
      {error, #{ code => ?LSP_ERROR_INTERNAL_ERROR, message => Message }, ServerState}
  end.

-spec ?REQUEST_SPEC('buildTarget/sources', buildTargetSourcesParams(), buildTargetSourcesResult()).
'buildTarget/sources'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
  Items = [ #{ target => Target
             , sources => target_sourceItems(Target, R3State) 
             } || Target <- Targets ],
  {response, #{items => Items}, ServerState}.

-spec ?REQUEST_SPEC('buildTarget/dependencySources', dependencySourcesParams(), dependencySourcesResult()).
'buildTarget/dependencySources'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
  Items = [ #{ target => Target
             , sources => target_dependencySources(Target, R3State) 
             } || Target <- Targets],
  {response, #{items => Items}, ServerState}.

-spec ?REQUEST_SPEC('buildTarget/compile', compileParams(), compileResult()).
'buildTarget/compile'(#{targets := Targets}, #{rebar3_state := R3State} = ServerState) ->
  F = fun(Target, AccR3State) ->
          case target_compile(Target, AccR3State) of
            {ok, NewR3State} ->
              NewR3State;
            Err ->
              ?LOG_CRITICAL("Error compiling ~p: ~p", [Target, Err]),
              AccR3State
          end
      end,
  NewR3State = lists:foldl(F, R3State, Targets),
  {response, #{ statusCode => 0 }, ServerState#{ rebar3_state => NewR3State }}.

-spec ?REQUEST_SPEC('buildTarget/test', testParams(), testResult()).
'buildTarget/test'(#{targets := Targets} = _Params, #{rebar3_state := R3State} = ServerState) ->
  [ target_test(Target, R3State) || Target <- Targets ],
  {response, #{ statusCode => 0 }, ServerState}.

-spec ?REQUEST_SPEC('rebar3/run', rebar3RunParams(), rebar3RunResult()).
'rebar3/run'(#{ args := Args }, #{ rebar3_state := R3State } = ServerState) ->
  case run(Args, R3State) of
    {ok, NewR3State} ->
      {response, #{}, ServerState#{ rebar3_state => NewR3State }};
    {{error, Reason}, _NewR3State} ->
      Message = io_lib:format("~p", [Reason]),
      {error, #{ code => ?LSP_ERROR_INTERNAL_ERROR, message => Message }, ServerState}
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec target_sourceItems(buildTargetIdentifier(), rebar_state:t()) -> [sourceItem()].
target_sourceItems(#{ uri := TargetUri }, R3State) ->
  case rebar3_bsp_uri:parse(TargetUri) of
    #{ scheme := <<"profile">>, path := Profile } ->
      ProjectApps = rebar_state:project_apps(R3State),
      ProfileApps = apps_with_profile(rebar3_bsp_util:to_atom(Profile), ProjectApps),
      [ #{ uri => rebar3_bsp_uri:dir(rebar_app_info:dir(App))
         , kind => ?SOURCE_ITEM_KIND_DIR
         , generated => false
         } || App <- ProfileApps ]
  end.

-spec target_dependencySources(buildTargetIdentifier(), rebar_state:t()) -> [uri()].
target_dependencySources(#{ uri := TargetUri }, R3State) ->
  case rebar3_bsp_uri:parse(TargetUri) of
    #{ scheme := <<"profile">>, path := Profile } ->
      AllDeps = rebar_state:all_deps(R3State),
      ProfileDeps = apps_with_profile(rebar3_bsp_util:to_atom(Profile), AllDeps),
      [ rebar3_bsp_uri:dir(rebar_app_info:dir(App)) || App <- ProfileDeps ]
  end.

-spec target_compile(buildTargetIdentifier(), rebar3_state:t()) -> {ok, rebar_state:t()} | {{error, term()}, rebar_state:t()}.
target_compile(#{ uri := TargetUri }, R3State) ->
  case rebar3_bsp_uri:parse(TargetUri) of
    #{ scheme := <<"profile">>, path := Profile } ->
      run([as, Profile, do, "compile"], R3State)
  end.

-spec target_test(buildTargetIdentifier(), rebar3_state:t()) -> {ok, rebar_state:t()} | {{error, term()}, rebar_state:t()}.
target_test(#{ uri := TargetUri }, R3State) ->
  case rebar3_bsp_uri:parse(TargetUri) of
    #{ scheme := <<"profile">>, path := Profile, query := Query } ->
      Args = [ io_lib:format("~p=~p", [K, V]) || {K,V} <- uri_string:dissect_query(Query) ],
      StrArgs = [ rebar3_bsp_util:to_string(A) || A <- Args ],
      run([as, Profile, do, "ct"] ++ StrArgs, R3State);
    #{ scheme := <<"profile">>, path := Profile } ->
      run([as, Profile, do, "ct"], R3State)
  end.

-spec run(list(), rebar_state:t()) -> {ok, rebar_state:t()} | {{error, term()}, rebar_state:t()}.
run(RawArgs, R3State) ->
  OldCodePath = code:get_path(),
  {Status, NewR3State} = try
                           Args = [rebar3_bsp_util:to_string(A) || A <- RawArgs],
                           CmdState0 = refresh_state(R3State),
                           CmdState = rebar_state:set(CmdState0, caller, api),
                           case rebar3:run(R3State, Args) of
                             {ok, TmpState} ->
                               {ok, TmpState};
                             {error, Err} when is_list(Err) ->
                               {{error, lists:flatten(Err)}, CmdState};
                             {error, Err} ->
                               {{error, Err}, CmdState}
                           end
                         catch
                           T:R:S ->
                             ?LOG_CRITICAL("BSP Stacktrace: ~p", [S]),
                             {{error, {T, R}}, R3State}
                         end,
  %% Unfortunately rebar3 messes with the code paths when running commands
  %% via rebar3:run/2 - it unsets deps and plugins paths. There is API to
  %% set these in rebar_paths:set_paths/2, but unfortunately it seems to
  %% be buggy - in the BSP unit tests we end up deeply nested like so:
  %% /Users/louai/github/al-khanji/rebar3_bsp/priv/sample/_build/default/plugins/rebar3_bsp/priv/sample/_build/default/plugins/rebar3_bsp/priv/sample/_build/default/plugins/rebar3_bsp/priv/sample/_build/default/plugins/rebar3_bsp/priv/sample/_build/default/plugins/rebar3_bsp/priv/sample/_build/default/plugins/rebar3_bsp/priv/sample
  %% To work around this for now we save the old code path above and simply
  %% add every entry again here *to the end of the path*.
  [ code:add_path(P) || P <- OldCodePath ],
  {Status, NewR3State}.

-spec refresh_state(rebar_state:t()) -> rebar_state:t().
refresh_state(R3State) ->
  InitConfig = rebar3:init_config(),
  rebar_state:apply_profiles(InitConfig,
                             rebar_state:current_profiles(R3State)).
-spec apps_with_profile(atom(), [rebar_app_info:t()]) -> [rebar_app_info:t()].
apps_with_profile(Profile, Apps) ->
  [ App || App <- Apps, lists:member(Profile, rebar_app_info:profiles(App)) ].

