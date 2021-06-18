%%==============================================================================
%% The Build Server Protocol
%%==============================================================================
-module(rebar3_bsp_protocol).
-behavior(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

%% Protocol Participant (either end) API
-export([ start_link/5
        , start_link/4
        ]).

%% gen_server API
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        , terminate/2
        , code_change/3
        , format_status/2
        ]).

%% Message API
-export([ message_type/1 ]).

%% Encoding API
-export([ notification/2
        , request/3
        , response/2
        , error/2
        ]).

%% Decoding API
-export([ decode_content/1
        , decode_packets/1
        , decode_packet/1
        , decode_headers/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("rebar3_bsp.hrl").

%%==============================================================================
%% Protocol Participant API
%%==============================================================================
-type state() :: #{ mod := module()
                  , port := port() | pid()
                  , owned_port := boolean()
                  , buffer := binary()
                  , inner_state := term()
                  }.

start_link(ServerName, Module, Args, PortSpec, Options) ->
  gen_server:start_link(ServerName, ?MODULE, {Module, Args, PortSpec}, Options).

start_link(Module, Args, PortSpec, Options) ->
  gen_server:start_link(?MODULE, {Module, Args, PortSpec}, Options).

init({Module, Args, PortSpec}) ->
  {OwnedPort, Port} = init_port(PortSpec),
  OuterState = #{ mod => Module
                , port => Port
                , owned_port => OwnPort
                , buffer => <<>>
                },
  case Module:init(Args) of
    {ok, InnerState} ->
      {ok, OuterState#{ inner_state => InnerState }};
    {ok, InnerState, Options} ->
      {ok, OuterState#{ inner_state => InnerState }, Options};
    {stop, Reason} ->
      {stop, Reason};
    ignore ->
      ignore
  end.

init_port({reuse_port, Port}) ->
  {false, Port};
init_port({open_port, PortName, PortSettings}) ->
  {true, erlang:open_port(PortName, PortSettings)}.

fini_port(#{ port := Port, owned_port := true }) ->
  Port ! {self(), close},
  ok;
fini_port(_) ->
  ok.

handle_call(Request, From, OuterState) ->
  dispatch(handle_call, [Request, From], OuterState).

handle_cast(Request, OuterState) ->
  dispatch(handle_cast, [Request], OuterState).

handle_info({Port, {data, Data}}, #{ port := Port, buffer := Buffer } = OuterState) ->
  {noreply, OuterState#{buffer => <<Buffer/binary, Data/binary>>}, {continue, '$bsp_decode'}};
handle_info(Info, #{ mod := Mod} = OuterState) ->
  case erlang:function_exported(Mod, handle_info, 2) of
    true ->
      dispatch(handle_info, [Info], OuterState);
    false ->
      ?LOG_WARNING("Discarding unexpected message ~p", [Info]),
      {noreply, OuterState}
  end.

handle_continue('$bsp_decode', #{ buffer := Buffer } = OuterState) ->
  case ?MODULE:decode_packets(Buffer) of
    {ok, Messages, RestData} ->
      {noreply, OuterState#{ buffer => RestData }, {continue, {'$bsp_dispatch_messages', Messages}}};
    {error, Reason} ->
      {stop, Reason, OuterState}
  end;
handle_continue({'$bsp_dispatch_messages', []}, OuterState) ->
  {noreply, OuterState};
handle_continue({'$bsp_dispatch_messages', [M|Ms]}, OuterState) ->
  NewOuterState = handle_message(M, OuterState),
  {noreply, NewOuterState, {continue, {'$bsp_dispatch_messages', Ms}}};
handle_continue(Continue, OuterState) ->
  

terminate(Reason, #{ mod := Mod, inner_state := InnerState } = OuterState) ->
  case erlang:function_exported(Mod, terminate, 2) of
    true ->
      Mod:terminate(Reason, InnerState);
    false ->
      ok
  end,
  fini_port(OuterState),
  ok.
  
code_change(OldVsn, #{ mod := Mod, inner_state := InnerState } = OuterState, Extra) ->
  case catch Mod:code_change(OldVsn, InnerState, Extra) of
    {ok, NewInnerState} ->
      {ok, OuterState#{ inner_state => NewInnerState }};
    Else ->
      Else
  end.

format_status(Opt, [PDict, #{ mod := Mod, inner_state := InnerState } = OuterState]) ->
  FormattedInnerState = case erlang:function_exported(Mod, format_status, 2) of
                          true ->
                            Mod:format_status(Opt, [PDict, InnerState]);
                          false ->
                            InnerState
                        end,
  OuterState#{ inner_state => FormattedInnerState, rebar3_state => rebar3_state }.

dispatch(Func, Args, #{ mod := Mod, inner_state := InnerState } = OuterState) ->
  case erlang:apply(Mod, Func, Args ++ [InnerState]) of
    {reply, Reply, NewInnerState} ->
      {reply, Reply, OuterState#{ inner_state => NewInnerState }};
    {reply, Reply, NewInnerState, Options} ->
      {reply, Reply, OuterState#{ inner_state => NewInnerState }, Options};
    {noreply, NewInnerState} ->
      {noreply, OuterState#{ inner_state => NewInnerState }};
    {noreply, NewInnerState, Options} ->
      {noreply, OuterState#{ inner_state => NewInnerState }, Options};
    {stop, Reason, Reply, NewInnerState} ->
      {stop, Reason, Reply, OuterState#{ inner_state => NewInnerState }};
    {stop, Reason, NewInnerState} ->
      {stop, Reason, OuterState#{ inner_state => NewInnerState }};
    Result ->
      Result
  end.
    

%%==============================================================================
%% Message API
%%==============================================================================
message_type(Message) ->
  case Message of
    %% Request - always has id and method
    #{ id := _Id, method := _Method } ->
      request;
    %% Response - always has id but never has a method
    #{ id := _Id } ->
      response;
    %% Notification - doesn't have an id, always has a method
    #{ method := _Method } ->
      notification
  end.  

%%==============================================================================
%% Encoding API
%%==============================================================================
-spec notification(binary(), params()) -> binary().
notification(Method, Params) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , method  => Method
   , params  => Params
   }.

-spec request(requestId(), binary(), params()) -> binary().
request(RequestId, Method, Params) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , id      => RequestId
   , method  => Method
   , params  => Params
   }.


-spec response(responseId(), responseResult()) -> binary().
response(RequestId, Result) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , id      => RequestId
   , result  => Result
   }.

-spec error(responseId(), responseError()) -> binary().
error(RequestId, Error) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , id      => RequestId
   , error   => Error
   }.

%%==============================================================================
%% Decoding API
%%==============================================================================
-spec decode_content(binary()) -> map().
decode_content(Content) ->
  jsx:decode(Content, [return_maps, {labels, atom}]).

-spec decode_packets(binary()) -> {[binary()], binary()}.
decode_packets(Data) ->
  decode_packets(Data, []).

-spec decode_packets(binary(), [binary()]) -> {[binary()], binary()}.
decode_packets(Data, Packets) ->
  case decode_packet(Data) of
    {ok, Packet, Rest} ->
      decode_packets(Rest, [Packet|Packets]);
    {more, _More} ->
      {ok, lists:reverse(Packets), Data};
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_packet(binary()) -> {ok, map(), binary()} | {more, undefined | integer()} | {error, term()}.
decode_packet(Data) ->
  case decode_headers(Data) of
    {ok, #{'Content-Length' := BinaryLength} = _Headers, Rest} ->
      Length = erlang:binary_to_integer(BinaryLength),
      case Rest of
        <<Content:Length/binary, FinalTail/binary>> ->
          {ok, decode_content(Content), FinalTail};
        _ ->
          {more, Length - erlang:byte_size(Rest)}
      end;
    {more, More} ->
      {more, More};
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_headers(binary()) -> {ok, map(), binary()} | {more, undefined | integer()} | {error, term()}.
decode_headers(Data) ->
  decode_headers(Data, #{}).

-spec decode_headers(binary(), map()) -> {ok, map(), binary()} | {more, undefined | integer()} | {error, term()}.
decode_headers(Data, Headers) ->
  case erlang:decode_packet(httph_bin, Data, []) of
    {more, More} ->
      {more, More};
    {error, Reason} ->
      {error, Reason};
    {ok, http_eoh, Rest} ->
      {ok, Headers, Rest};
    {ok, {http_header, _Bit, Field, _UnmodifiedField, Value}, Rest} ->
      decode_headers(Rest, Headers#{Field => Value})
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec content(binary()) -> binary().
content(Body) ->
  unicode:characters_to_binary([headers(Body), "\r\n", Body]).

-spec headers(binary()) -> iolist().
headers(Body) ->
  io_lib:format("Content-Length: ~p\r\n", [byte_size(Body)]).
