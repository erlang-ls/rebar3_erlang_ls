-module(rebar3_bsp_util).

-export([ bring_up_local_client_server/1
        , tear_down_local_client_server/1
        , maybe_stop/2
        , to_binary/1
        , to_string/1
        , to_atom/1
        , map_fread/3
        , lists_intersection/1
        , lists_union/1
        , cd/1
        , sample_app_dir/0
        , clean_sample_app_dir/0
        ]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

-define(SETS, sets).

-spec bring_up_local_client_server(rebar_state:t()) -> {ok, {pid(), pid(), pid()}}.
bring_up_local_client_server(R3State) ->
  {ok, EchoPort} = rebar3_bsp_echo_port:start_link(),
  {ok, ClientPid} = rebar3_bsp_client:start_link({port, EchoPort}),
  {ok, ServerPid} = rebar3_bsp_server:start_link(R3State, EchoPort),
  ok = rebar3_bsp_echo_port:set_endpoints(EchoPort, {ClientPid, ServerPid}),
  {ok, {EchoPort, ClientPid, ServerPid}}.

-spec tear_down_local_client_server({pid(), pid(), pid()}) -> ok.
tear_down_local_client_server({EchoPort, ClientPid, ServerPid}) ->
  ok = maybe_stop(ClientPid, rebar3_bsp_client),
  ok = maybe_stop(ServerPid, rebar3_bsp_server),
  ok = rebar3_bsp_echo_port:stop(EchoPort),
  ok.

-spec maybe_stop(pid(), atom()) -> ok.
maybe_stop(Pid, Name) ->
  case whereis(Name) of
    Pid ->
      Name:stop();
    _ ->
      ok
  end.

-spec to_atom(atom() | binary() | list()) -> atom().
to_atom(A) when is_atom(A) ->
  A;
to_atom(B) when is_binary(B) ->
  erlang:binary_to_atom(B, utf8);
to_atom(L) when is_list(L) ->
  erlang:list_to_atom(L);
to_atom(X) ->
  error(badarg, [X]).

-spec to_binary(atom() | binary() | list() | integer()) -> binary().
to_binary(A) when is_atom(A) ->
  erlang:atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) ->
  B;
to_binary(L) when is_list(L) ->
  case unicode:characters_to_binary(L) of
    Binary when is_binary(Binary) ->
      Binary;
    _ ->
      erlang:list_to_binary(L)
  end;
to_binary(I) when is_integer(I) ->
  erlang:integer_to_binary(I);
to_binary(X) ->
  error(badarg, [X]).

-spec to_string(atom() | binary() | list()) -> string().
to_string(A) when is_atom(A) ->
  to_string(erlang:atom_to_list(A));
to_string(B) when is_binary(B) ->
  case unicode:characters_to_list(B) of
    List when is_list(List) ->
      List;
    _ ->
      erlang:binary_to_list(B) 
  end;
to_string(L) when is_list(L) ->
  case unicode:characters_to_list(L) of
    List when is_list(List) ->
      List;
    _ ->
      L 
  end;
to_string(X) ->
  error(badarg, [X]).

-spec map_fread(term(), map(), string()) -> {ok, [io_lib:fread_item()], string()} | {error, term()}.
map_fread(Key, Map, Format) ->
  try
    Value = maps:get(Key, Map),
    StringVal = to_string(Value),
    case io_lib:fread(Format, StringVal) of
      {ok, Results, LeftOverChars} ->
        {ok, Results, LeftOverChars};
      {error, {fread, Error}} ->
        {error, {fread, Error}};
      {more, _RestFormat, _Nchars, _InputStack} = Error ->
        {error, {fread, Error}}
    end
  catch
    error:{badkey, Key} ->
      {error, {badkey, Key}}
  end.

-spec lists_intersection([list()]) -> list().
lists_intersection(Lists) ->
  lists_set_operation(intersection, Lists).

-spec lists_union([list()]) -> list().
lists_union(Lists) ->
  lists_set_operation(union, Lists).

-spec lists_set_operation(atom(), [list()]) -> list().
lists_set_operation(Op, [_|_] = Lists) ->
  F = fun(Set, Acc) -> ?SETS:Op(Set, Acc) end,
  [H|T] = [ ?SETS:from_list(L) || L <- Lists ],
  Result = lists:foldl(F, H, T),
  ?SETS:to_list(Result).

-spec cd(file:filename() | binary()) ->
        {ok, file:filename() | undefined, file:filename()} |
        {error, file:posix() | badarg | no_translation}.
cd(To) ->
  From = case file:get_cwd() of
           {ok, Dir} ->
             Dir;
           Error ->
             ?LOG_ERROR("file:get_cwd/0 => ~p", [Error]),
             undefined
         end,
  case file:set_cwd(To) of
    ok ->
      {ok, From, To};
    {error, Reason} ->
      {error, Reason}
  end.

-spec sample_app_dir() -> file:name().
sample_app_dir() ->
  PrivDir = code:priv_dir(rebar3_bsp),
  ResolvedPrivDir = rebar_file_utils:resolve_link(PrivDir),
  Result = filename:join([ResolvedPrivDir, "sample"]),
  Result.

-spec clean_sample_app_dir() -> ok.
clean_sample_app_dir() ->
  SAD = sample_app_dir(),
  Targets = [ filename:join([SAD, D]) || D <- [".bsp", "_build", "rebar.lock"] ],
  F = fun(Target) ->
          case file:read_file_info(Target) of
            {ok, #file_info{ type = directory }} ->
              ok = rebar_file_utils:rm_rf(Target);
            {ok, #file_info{ type = regular }} ->
              ok = file:delete(Target);
            {ok, #file_info{ type = Type }} ->
              ?LOG_CRITICAL("don't know how handle file type ~p: ~p", [Type, Target]);
            {error, enoent} ->
              ok;
            {error, Reason} ->
              ?LOG_CRITICAL("can't clean out ~p: ~p", [Target, file:format_error(Reason)])
          end
      end,
  ok = lists:foreach(F, Targets),
  ok.

