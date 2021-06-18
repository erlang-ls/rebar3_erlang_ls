%%==============================================================================
%% The Language Server Protocol
%%==============================================================================
-module(rebar3_bsp_protocol).

%%==============================================================================
%% Exports
%%==============================================================================

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
%% Encoding API
%%==============================================================================
-spec notification(binary(), params()) -> binary().
notification(Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , params  => Params
             },
  content(jsx:encode(Message)).

-spec request(requestId(), binary(), params()) -> binary().
request(RequestId, Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , id      => RequestId
             , method  => Method
             , params  => Params
             },
  content(jsx:encode(Message)).

-spec response(responseId(), responseResult()) -> binary().
response(RequestId, Result) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , id      => RequestId
             , result  => Result
             },
  content(jsx:encode(Message)).

-spec error(responseId(), responseError()) -> binary().
error(RequestId, Error) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , id      => RequestId
             , error   => Error
             },
  content(jsx:encode(Message)).

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
