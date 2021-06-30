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
        , encode_json/1
        , decode_json/1
        , peel_message/1
        , peel_content/1
        , peel_headers/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("rebar3_bsp.hrl").

%%==============================================================================
%% Definitions
%%==============================================================================
-define(CONTENT_LENGTH, 'Content-Length').

%%==============================================================================
%% Types
%%==============================================================================
-type headers()      :: map().
-type buffer()       :: binary().
-type more()         :: {more, undefined | non_neg_integer()}.
-type decode_error() :: {error, term(), buffer()}.

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

-spec message_id(map()) -> requestId() | null.
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
-spec send_message(port() | pid(), jsx:json_term()) -> ok.
send_message(Port, Message) when is_port(Port) ->
  true = port_command(Port, encode_json(Message)),
  ok;
send_message(Port, Message) when is_pid(Port) ->
  Port ! {self(), {command, encode_json(Message)}},
  ok.

-spec encode_json(jsx:json_term()) -> binary().
encode_json(Content) ->
  try
    content(jsx:encode(Content))
  catch
    error:badarg ->
      error({badarg, Content})
  end.

-spec decode_json(binary()) -> jsx:json_term().
decode_json(Content) ->
  try
    jsx:decode(Content, [return_maps, {labels, atom}])
  catch
    error:badarg ->
      error({badarg, Content})
  end.

-spec peel_message(buffer()) -> {ok, jsx:json_term(), buffer()} | more() | decode_error().
peel_message(Buffer) ->
  case peel_content(Buffer) of
    {ok, Content, Rest} ->
      try
        {ok, decode_json(Content), Rest}
      catch
        error:{badarg, Content} ->
          {error, {{badjson, Content}, #{buffer => Buffer}}, Rest}
      end;
    {more, More} ->
      {more, More};
    {error, {Reason, ErrorInfoMap}, Rest} ->
      {error, {Reason, ErrorInfoMap#{buffer => Buffer}}, Rest}
  end.

-spec peel_content(buffer()) -> {ok, binary(), buffer()} | more() | decode_error().
%% We progressively peel parts off the front of the input buffer. In the happy
%% case we expect to have <<Headers/binary, Content/binary, Rest/binary>> as input.
%% The buffers are named to reflect their expected content: as we chop bits
%% off, words are dropped from the front.
peel_content(HeadersContentRest) ->
  case peel_headers(HeadersContentRest, #{}) of
    {ok, Headers, ContentRest} ->
      case rebar3_bsp_util:map_fread(?CONTENT_LENGTH, Headers, "~u") of
        {ok, [ContentLength], _TrailingGarbage} ->
          case ContentRest of
            <<Content:ContentLength/binary, Rest/binary>> ->
              {ok, Content, Rest};
            ContentRest ->
              {more, ContentLength - erlang:byte_size(ContentRest)}
          end;
        {error, Reason} ->
          {error, {{badheaders, Reason}, #{headers => Headers}}, ContentRest}
      end;
    {more, More} ->
      {more, More};
    {error, Reason, Rest} ->
      {error, Reason, Rest}
  end.

-spec peel_headers(buffer(), headers()) -> {ok, headers(), buffer()} | more() | decode_error().
peel_headers(Buffer, Headers) ->
  case erlang:decode_packet(httph_bin, Buffer, []) of
    {ok, {http_header, _Bit, Field, _UnmodifiedField, Value}, Rest} ->
      peel_headers(Rest, Headers#{ Field => Value});
    {ok, http_eoh, Rest} ->
      {ok, Headers, Rest};
    {more, More} ->
      {more, More};
    {ok, {http_error, HttpError}, Rest} ->
      {error, {{http_error, HttpError}, #{headers => Headers}}, Rest};
    {error, Reason} ->
      {error, {badheaders, Reason}, Buffer}
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec content(binary()) -> binary().
content(Body) ->
  rebar3_bsp_util:to_binary([headers(Body), "\r\n", Body]).

-spec headers(binary()) -> iolist().
headers(Body) ->
  io_lib:format("Content-Length: ~p\r\n", [byte_size(Body)]).

