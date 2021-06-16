-module(rebar3_bsp_methods).

-export([ build_initialize/2
        , build_initialized/2
        , workspace_buildtargets/2
        , buildtarget_compile/2
        , buildtarget_sources/2
        , buildtarget_dependencysources/2
        ]).

-export([ custom_format/2
        ]).

-include("rebar3_bsp.hrl").

-spec build_initialize(initializeBuildParams(), rebar3_state:t()) ->
        {initializeBuildResult(), rebar3_state:t()}.
build_initialize(_Params, State) ->
  Result = #{ displayName => <<"rebar3_bsp">>
            , version => version()
            , bspVersion => ?BSP_VSN
            , capabilities => #{}
            },
  {Result, State}.

-spec build_initialized(initializedBuildParams(), rebar3_state:t()) ->
        {ok, rebar3_state:t()}.
build_initialized(#{}, State) ->
  {ok, NewState} = rebar3:run(State, ["compile"]),
  {ok, NewState}.

-spec workspace_buildtargets(
        workspaceBuildTargetsParams(), rebar3_state:t()
       ) -> {workspaceBuildTargetsResult(), rebar3_state:t()}.
workspace_buildtargets(#{}, State) ->
  Profiles = rebar_state:current_profiles(State),
  %% TODO: Targets should be of the form #{uri => binary()}
  Targets = [#{id => atom_to_binary(P, utf8)} || P <- Profiles],
  {#{ targets => Targets }, State}.

-spec buildtarget_compile(compileParams(), rebar3_state:t()) ->
        {compileResult(), rebar3_state:t()}.
buildtarget_compile(_Params, State) ->
  %% TODO: Hard-coded filename
  {ok, NewState} = rebar3:run(State, ["erl_subgraph_compile", "-f", "src/sample_app.erl"]),
  %% TODO: Compile test application and publish diagnostics
  %% TODO: Macros for statusCode
  {#{ statusCode => 0 }, NewState}.

-spec buildtarget_sources(buildTargetSourcesParams(), rebar3_state:t()) ->
        {buildTargetSourcesResult(), rebar3_state:t()}.
buildtarget_sources(#{ targets := Targets }, State) ->
  Items = items(rebar_state:project_apps(State), Targets),
  {#{ items => Items }, State}.

-spec buildtarget_dependencysources(dependencySourcesParams(), rebar3_state:t()) ->
        {dependencySourcesResult(), rebar3_state:t()}.
buildtarget_dependencysources(#{ targets := Targets }, State) ->
  Items = items(rebar_state:all_deps(State), Targets),
  {#{ items => Items }, State}.

-spec custom_format(map(), rebar_state:t()) -> {map(), rebar_state:t()}.
custom_format(Params, State) ->
  #{output := Output, file := File} = Params,
  Args = ["format", "-o", binary_to_list(Output), "-f", binary_to_list(File)],
  case rebar3:run(State, Args) of
    {ok, _NewState} ->
      ok = rebar_paths:set_paths([deps, plugins], State),
      {#{}, State};
    {error, Reason} ->
      {#{<<"error">> => unicode:characters_to_binary(Reason)}, State}
  end.

%% Internal Functions

-spec version() -> binary().
version() ->
  {ok, Vsn} = application:get_key(rebar3_bsp, vsn),
  list_to_binary(Vsn).

-spec items([atom()], [binary()]) -> [uri()].
items(Sources, Targets) ->
  Apps = [A || A <- Sources, belongs_to_targets(Targets, A)],
  [rebar_app_info:dir(A) || A <- Apps].

-spec belongs_to_targets([binary()], rebar_app_info:t()) -> boolean().
belongs_to_targets(Targets, A) ->
  Profiles = rebar_app_info:profiles(A),
  lists:any(fun(T) -> lists:member(binary_to_atom(T, utf8), Profiles) end, Targets).
