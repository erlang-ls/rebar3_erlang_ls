-module(rebar3_bsp_util).

-export([ client_request/2
        , client_request/3
        , client_notify/2
        , initialize_server/0
        , bring_up_local_client_server/1
        , tear_down_local_client_server/1
        , maybe_stop/2
        , new_rebar_state_from_file/1
        , to_binary/1
        , to_string/1
        , to_atom/1
        , map_fread/3
        , lists_intersection/1
        , lists_union/1
        , cd/1
        ]).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, 5000).
-define(SETS, sets).

-spec client_request(binary() | atom(), map()) -> any().
client_request(Method, Params) ->
  client_request(Method, Params, ?TIMEOUT).

-spec client_request(binary() | atom(), map(), timeout()) -> any().
client_request(Method, Params, Timeout) ->
  RequestId = rebar3_bsp_client:send_request(Method, Params),
  rebar3_bsp_client:receive_response(RequestId, Timeout).

-spec client_notify(binary() | atom(), map()) -> ok.
client_notify(Method, Params) ->
  ok = rebar3_bsp_client:send_notification(Method, Params),
  ok.

-spec initialize_server() -> ok.
initialize_server() ->
  {ok, _Result} = client_request('build/initialize', #{}),
  ok = client_notify('build/initialized', #{}),
  ok.

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

-spec new_rebar_state_from_file(file:name()) -> rebar_state:t().
new_rebar_state_from_file(Filename) ->
  {ok, RebarConfig} = file:consult(Filename),
  rebar_state:new(RebarConfig).

-spec to_binary(atom() | binary() | list()) -> binary().
to_binary(A) when is_atom(A) ->
  atom_to_binary(A);
to_binary(B) when is_binary(B) ->
  B;
to_binary(L) when is_list(L) ->
  list_to_binary(L);
to_binary(X) ->
  error(badarg, [X]).

-spec to_string(atom() | binary() | list()) -> string().
to_string(A) when is_atom(A) ->
  atom_to_list(A);
to_string(B) when is_binary(B) ->
  binary_to_list(B);
to_string(L) when is_list(L) ->
  L;
to_string(X) ->
  error(badarg, [X]).

-spec to_atom(atom() | binary() | list()) -> atom().
to_atom(A) when is_atom(A) ->
  A;
to_atom(B) when is_binary(B) ->
  binary_to_atom(B);
to_atom(L) when is_list(L) ->
  list_to_atom(L);
to_atom(X) ->
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

