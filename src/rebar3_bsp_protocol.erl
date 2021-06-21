%%==============================================================================
%% The Build Server Protocol
%%==============================================================================
-module(rebar3_bsp_protocol).

%%==============================================================================
%% Exports
%%==============================================================================

%% Message API
-export([ message_type/1
        , send_message/2
        ]).

%% Encoding API
-export([ notification/2
        , request/3
        , response/2
        , error/2
        , encode_content/1
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

-spec send_message(port() | pid(), map()) -> ok.
send_message(Port, Message) when is_port(Port) ->
  true = port_command(Port, encode_content(Message)),
  ok;
send_message(Port, Message) when is_pid(Port) ->
  Port ! {self(), {command, encode_content(Message)}},
  ok.

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

-spec encode_content(map()) -> binary().
encode_content(Content) ->
  content(jsx:encode(Content)).

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
