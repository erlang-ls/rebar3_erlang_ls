-module(rebar3_bsp_protocol_SUITE).

%% CT Callbacks
-export([ all/0
        , groups/0
        ]).

%% Message API Test Cases
-export([ message_type/1
        , message_id/1
        , notification/1
        , request/1
        , response/1
        , error/1
        ]).

%% Content API Test Cases
-export([ send_message/1
        , encode_json/1
        , decode_json/1
        , peel_message/1
        , peel_content/1
        , peel_headers/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Definitions
%%==============================================================================
-define(SPEC(Name), -spec Name(ct_suite:ct_config()) -> term()).
-define(M, rebar3_bsp_protocol).
-define(JSONRPC_VSN, <<"2.0">>). %% Break tests if we change it elsewhere

%%==============================================================================
%% CT Callbacks
%%==============================================================================
all() ->
  [ {group, GroupName} || {GroupName, _, _} <- groups() ].

groups() ->
  [ { message_api
    , []
    , [ message_type
      , message_id
      , notification
      , request
      , response
      , error
      ]
    }
  , { content_api
    , []
    , [ send_message
      , encode_json
      , decode_json
      , peel_message
      , peel_content
      , peel_headers
      ]
    }
  ].

%%==============================================================================
%% Message API Test Cases
%%==============================================================================
?SPEC(message_type).
message_type(_Config) ->
  ?assertEqual(request, ?M:message_type(#{ id => foo, method => bar })),
  ?assertEqual(response, ?M:message_type(#{ id => foo })),
  ?assertEqual(notification, ?M:message_type(#{ method => foo })),
  ?assertError({case_clause, #{}}, ?M:message_type(#{})),
  ok.

?SPEC(message_id).
message_id(_Config) ->
  ?assertEqual(<<"foo">>, ?M:message_id(#{ id => <<"foo">> })),
  ?assertEqual(null, ?M:message_id(#{})),
  ok.

-spec jsonrpc_msg(map()) -> map().
jsonrpc_msg(Message) ->
  Message#{ jsonrpc => ?JSONRPC_VSN }.

?SPEC(notification).
notification(_Config) ->
  Method = <<"foo">>,
  Params = #{ bar => <<"baz">> },
  Expected = jsonrpc_msg(#{ method => Method, params => Params }),
  ?assertEqual(Expected, ?M:notification(Method, Params)),
  ok.

?SPEC(request).
request(_Config) ->
  RequestId = 16#cafe,
  Method = <<"foo">>,
  Params = #{ bar => <<"baz">> },
  Expected = jsonrpc_msg(#{ id => RequestId, method => Method, params => Params }),
  ?assertEqual(Expected, ?M:request(RequestId, Method, Params)),
  ok.

?SPEC(response).
response(_Config) ->
  RequestId = 16#cafe,
  Result = #{ great => <<"success">> },
  Expected = jsonrpc_msg(#{ id => RequestId, result => Result }),
  ?assertEqual(Expected, ?M:response(RequestId, Result)),
  ok.

?SPEC(error).
error(_Config) ->
  RequestId = 16#cafe,
  Error = #{ code => 16#dead, message => <<"failure">> },
  Expected = jsonrpc_msg(#{ id => RequestId, error => Error }),
  ?assertEqual(Expected, ?M:error(RequestId, Error)),
  ok.

%%==============================================================================
%% Content API Test Cases
%%==============================================================================
?SPEC(send_message).
send_message(_Config) ->
  TestMessage = ?M:notification(<<"foo">>, #{}),
  Self = self(),
  Sender = spawn(fun() -> ok = ?M:send_message(Self, TestMessage) end),
  receive
    {Sender, {command, EncodedTestMessage}} ->
      ?assertEqual(?M:encode_json(TestMessage), EncodedTestMessage)
  after 100 ->
      ct:fail(nomessage)
  end,
  ok.

-spec make_protocol_buffer(term()) -> binary().
make_protocol_buffer(Content) ->
  Encoded = jsx:encode(Content),
  Length = rebar3_bsp_util:to_binary(byte_size(Encoded)),
  <<"Content-Length: ", Length/binary, "\r\n\r\n", Encoded/binary>>.

?SPEC(encode_json).
encode_json(_Config) ->
  %% As a side effect we also test our helper function above
  ?assertEqual(make_protocol_buffer(#{ a=> null}), ?M:encode_json(#{ a => null })),
  ?assertEqual(<<"Content-Length: 10\r\n\r\n{\"a\":null}">>, ?M:encode_json(#{ a => null })),
  ?assertError({badarg, {}}, ?M:encode_json({})),
  ok.

?SPEC(decode_json).
decode_json(_Config) ->
  ?assertEqual([], ?M:decode_json(<<"[]">>)),
  ?assertEqual(#{}, ?M:decode_json(<<"{}">>)),
  ?assertEqual(<<"foo">>, ?M:decode_json(<<"\"foo\"">>)),
  ?assertError({badarg, <<"foo">>}, ?M:decode_json(<<"foo">>)),
  ?assertEqual(#{ foo => <<"bar">> }, ?M:decode_json(<<"{\"foo\":\"bar\"}">>)),
  ok.

?SPEC(peel_message).
peel_message(_Config) ->
  ?assertEqual({more, undefined}, ?M:peel_message(<<>>)),
  ?assertEqual({ok, #{ id => <<"foo">> }, <<>>}, ?M:peel_message(make_protocol_buffer(#{ id => foo}))),
  Head = <<"h:::\r\n">>,
  Garbage = <<"\0\0\0abc\r\n">>,
  Tail = <<"tail">>,
  BadBuf = <<Head/binary, Garbage/binary, Tail/binary>>,
  ?assertEqual({error,
                {{http_error, Garbage}, #{ headers => #{ <<"H">> => <<"::">> },
                                           buffer => BadBuf}},
                Tail},
               ?M:peel_message(BadBuf)),
  ?assertMatch({error, {{badheaders, {badkey, 'Content-Length'}}, #{}}, <<>>},
               ?M:peel_message(<<Head/binary, "\r\n">>)),
  ok.

?SPEC(peel_content).
peel_content(_Config) ->
  ?assertEqual({more, undefined}, ?M:peel_content(<<>>)),
  ?assertMatch({error, {{badheaders, _}, _}, <<"data">>},
               ?M:peel_content(<<"Some-Header: x\r\n\r\ndata">>)),
  ?assertEqual({ok, <<"data">>, <<>>}, ?M:peel_content(<<"Content-Length: 4\r\n\r\ndata">>)),
  ok.

?SPEC(peel_headers).
peel_headers(_Config) ->
  ?assertEqual({ok, #{ a => b}, <<>>}, ?M:peel_headers(<<"\r\n">>, #{ a => b })),
  ?assertEqual({more, undefined}, ?M:peel_headers(<<>>, #{})),
  ?assertEqual({more, undefined}, ?M:peel_headers(<<"foo: bar\r\n">>, #{})),
  ?assertEqual({more, undefined}, ?M:peel_headers(<<"\r\r\n">>, #{})),
  ?assertEqual({error, {{http_error, <<"\r\r\n">>}, #{headers => #{}}}, <<"\r\n">>},
               ?M:peel_headers(<<"\r\r\n\r\n">>, #{})),
  ok.

