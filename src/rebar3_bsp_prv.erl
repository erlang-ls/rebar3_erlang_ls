-module(rebar3_bsp_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, bsp).
-define(DEPS, []).
-define(AGENT, rebar3_bsp_server).
-define(NAME_PREFIX, "rebar3_bsp_").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Opts = [{generate, undefined, "generate", boolean, "Generate the BSP Connection file and exit"}],
  Provider = providers:create([
                               {name, ?PROVIDER},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, ?DEPS},
                               {example, "rebar3 bsp"},
                               {opts, Opts},
                               {short_desc, "Build Server Protocol (BSP) plugin"},
                               {desc, "Plugin adding Build Server Protocol (BSP) support for rebar3"},
                               {profiles, [test]}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  case proplists:is_defined(generate, Args) of
    true ->
      Dir = rebar_state:dir(State),
      rebar3_bsp_connection:generate(Dir);
    false ->
      setup_name(State),
      start_server(State)
  end,
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec start_server(rebar_state:t()) -> no_return().
start_server(State) ->
  process_flag(trap_exit, true),
  {ok, _AppNames} = application:ensure_all_started(rebar3_bsp, permanent),
  {ok, Pid} = supervisor:start_child(rebar3_bsp_sup, #{ id => rebar3_bsp_server
                                                      , start => { rebar3_bsp_server
                                                                 , start_link
                                                                 , [State]
                                                                 }
                                                      , restart => temporary
                                                      }),
  link(Pid),
  init ! {'EXIT', self(), normal},
  wait_loop(Pid).

-spec wait_loop(pid()) -> no_return().
wait_loop(ServerPid) ->
  receive
    {'EXIT', ServerPid, Reason} ->
      case Reason of
        {exit_code, ExitCode} ->
          init:stop(ExitCode);
        _ ->
          init:stop()
      end;
    _ ->
      ok
  end,
  wait_loop(ServerPid).

-spec setup_name(rebar_state:t()) -> ok.
setup_name(State) ->
  {_Long, Short, Opts} = rebar_dist_utils:find_options(State),
  Name = case Short of
           undefined ->
             rebar3_bsp_util:to_atom(filename:basename(rebar_state:dir(State)));
           N ->
             N
         end,
  Int = erlang:phash2(erlang:timestamp()),
  Id = lists:flatten(io_lib:format("~s_~p_~p", [?NAME_PREFIX, Name, Int])),
  rebar_dist_utils:short(rebar3_bsp_util:to_atom(Id), Opts),
  ok.

