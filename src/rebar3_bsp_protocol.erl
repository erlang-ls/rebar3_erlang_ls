%%==============================================================================
%% The Build Server Protocol
%%==============================================================================
-module(rebar3_bsp_protocol).

%%==============================================================================
%% Exports
%%==============================================================================

%% Message API
-export([ message_type/1
        , message_id/1
        , notification/2
        , request/3
        , response/2
        , error/2
        ]).

%% Content API
-export([ send_message/2
        , encode_content/1
        , decode_content/1
        , carefully_decode_packets/1
        , decode_packets/1
        , decode_packet/1
        , decode_headers/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("rebar3_bsp.hrl").

%%==============================================================================
%% Message API
%%==============================================================================
-spec message_type(map()) -> request | response | notification.
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

-spec message_id(map()) -> requestId().
message_id(Message) ->
  case Message of
    #{ id := Id } ->
      Id;
    _ ->
      null
  end.

-spec notification(binary(), params()) -> map().
notification(Method, Params) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , method  => Method
   , params  => Params
   }.

-spec request(requestId(), binary(), params()) -> map().
request(RequestId, Method, Params) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , id      => RequestId
   , method  => Method
   , params  => Params
   }.


-spec response(responseId(), responseResult()) -> map().
response(RequestId, Result) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , id      => RequestId
   , result  => Result
   }.

-spec error(responseId(), responseError()) -> map().
error(RequestId, Error) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , id      => RequestId
   , error   => Error
   }.

%%==============================================================================
%% Content API
%%==============================================================================
-spec send_message(port() | pid(), map()) -> ok.
send_message(Port, Message) when is_port(Port) ->
  true = port_command(Port, encode_content(Message)),
  ok;
send_message(Port, Message) when is_pid(Port) ->
  Port ! {self(), {command, encode_content(Message)}},
  ok.

-spec encode_content(map()) -> binary().
encode_content(Content) ->
  content(jsx:encode(Content)).

-spec decode_content(binary()) -> map().
decode_content(Content) ->
  jsx:decode(Content, [return_maps, {labels, atom}]).

-spec carefully_decode_packets(binary()) -> {[map()], binary()}.
carefully_decode_packets(Data) ->
  case decode_packets(Data) of
    {ok, Messages, RestData} ->
      {Messages, RestData};
    {error, Reason, Messages, RestData} ->
      ?LOG_CRITICAL("Decode error encountered, trying to continue. [error=~p]", Reason),
      {Messages, RestData}
  end.

-spec decode_packets(binary()) -> {ok, [map()], binary()} | {error, term(), [map()], binary()}.
decode_packets(Data) ->
  decode_packets(Data, []).

-spec decode_packets(binary(), [map()]) -> {ok, [map()], binary()} | {error, term(), [map()], binary()}.
decode_packets(Data, Packets) ->
  case decode_packet(Data) of
    {ok, Packet, Rest} ->
      decode_packets(Rest, [Packet|Packets]);
    {more, _More} ->
      {ok, lists:reverse(Packets), Data};
    {error, Reason, Rest} ->
      {error, Reason, lists:reverse(Packets), Rest}
  end.

-spec decode_packet(binary()) -> {ok, map(), binary()} | {more, undefined | integer()} | {error, term(), binary()}.
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
    {ok, #{}, Rest} ->
      {error, noheaders, Rest};
    {more, More} ->
      {more, More};
    {error, Reason} ->
      {error, Reason, Data}
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
