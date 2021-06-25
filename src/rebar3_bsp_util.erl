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
        , map_fread/3
        ]).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, 5000).

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
  {ok, ServerPid} = rebar3_bsp_server:start_link(#{ rebar3_state => R3State
                                                  , port => EchoPort }),
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
  list_to_binary(L).

-spec to_string(atom() | binary() | list()) -> string().
to_string(A) when is_atom(A) ->
  atom_to_list(A);
to_string(B) when is_binary(B) ->
  binary_to_list(B);
to_string(L) when is_list(L) ->
  L.

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

