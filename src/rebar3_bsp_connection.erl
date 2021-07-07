%%==============================================================================
%% BSP Connection
%%==============================================================================
-module(rebar3_bsp_connection).
-include("rebar3_bsp.hrl").

%%==============================================================================
%% Exports
%%==============================================================================
-export([ generate/1
        , discover/1
        , version/1
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type details() :: #{ name => binary()
                    , version => binary()
                    , bspVersion => binary()
                    , languages => [binary()]
                    , argv => [binary()]
                    }.
-export_type([details/0]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(NAME      , <<"rebar3">>).
-define(LANGUAGES , [<<"erlang">>]).
-define(ARGS      , [?NAME, <<"bsp">>]).
-define(BSP_DIR   , <<".bsp">>).
-define(FILENAME  , <<?NAME/binary, ".json">>).

%%==============================================================================
%% API
%%==============================================================================
-spec generate(string()) -> ok.
generate(BaseDir) ->
  Path = filepath(BaseDir),
  ok = filelib:ensure_dir(Path),
  Details = jsx:encode(details(), [space, indent]),
  ok = file:write_file(Path, <<Details/binary, "\n">>).

-spec discover(string()) -> {ok, string(), [string()]}.
discover(BaseDir) ->
  [C|_] = filelib:wildcard(filename:join([BaseDir, ".bsp", "*.json"])),
  {ok, Content} = file:read_file(C),
  #{ argv := Argv } = jsx:decode(Content, [return_maps, {labels, atom}]),
  [Cmd|Params] = [rebar3_bsp_util:to_string(X) || X <- Argv],
  E = os:find_executable(Cmd),
  {ok, E, Params}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec details() -> details().
details() ->
  LauncherPath = filename:join([code:priv_dir(?BSP_APPLICATION), ?BSP_LAUNCHER]),
  Launcher = rebar3_bsp_util:to_binary(LauncherPath),
  #{ name       => ?NAME
   , version    => version(?BSP_APPLICATION)
   , bspVersion => ?BSP_VSN
   , languages  => ?LANGUAGES
   , argv       => [Launcher|?ARGS]
   }.

-spec filepath(file:filename()) -> file:filename().
filepath(BaseDir) ->
  Dir = filename:join([BaseDir, ?BSP_DIR]),
  filename:join([Dir, ?FILENAME]).

-spec version(atom()) -> binary().
version(Application) ->
  {ok, Vsn} = application:get_key(Application, vsn),
  rebar3_bsp_util:to_binary(Vsn).
