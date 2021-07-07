%%==============================================================================
%% Global Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(JSONRPC_VSN, <<"2.0">>).
-define(BSP_VSN, <<"2.0.0">>).

-define(BSP_APPLICATION, rebar3_bsp).
-define(BSP_LAUNCHER, "rebar3_bsp_launcher.sh").

-define(SOURCE_ITEM_KIND_FILE, 1).
-define(SOURCE_ITEM_KIND_DIR, 2).

-if(?OTP_RELEASE > 23).
-define(HAVE_GEN_SERVER_SEND_REQUEST, true).
-endif.

-ifdef(HAVE_GEN_SERVER_SEND_REQUEST).
-define(GEN_SERVER_SEND_REQUEST, gen_server:send_request).
-define(GEN_SERVER_WAIT_RESPONSE, gen_server:wait_response).
-define(GEN_SERVER_CHECK_RESPONSE, gen_server:check_response).
-define(GEN_SERVER_RECEIVE_RESPONSE, gen_server:receive_response).
-else.
-define(GEN_SERVER_SEND_REQUEST, rebar3_bsp_gen_server:send_request).
-define(GEN_SERVER_WAIT_RESPONSE, rebar3_bsp_gen_server:wait_response).
-define(GEN_SERVER_CHECK_RESPONSE, rebar3_bsp_gen_server:check_response).
-define(GEN_SERVER_RECEIVE_RESPONSE, rebar3_bsp_gen_server:receive_response).
-endif.

%%==============================================================================
%% Error codes from LSP Specification
%%==============================================================================

%% // Defined by JSON RPC
-define(LSP_ERROR_PARSE_ERROR,                        -32700).
-define(LSP_ERROR_INVALID_REQUEST,                    -32600).
-define(LSP_ERROR_METHOD_NOT_FOUND,                   -32601).
-define(LSP_ERROR_INVALID_PARAMS,                     -32602).
-define(LSP_ERROR_INTERNAL_ERROR,                     -32603).

%% /**
%% * This is the start range of JSON RPC reserved error codes.
%% * It doesn't denote a real error code. No LSP error codes should
%% * be defined between the start and end range. For backwards
%% * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
%% * are left in the range.
%% *
%% * @since 3.16.0
%% */
-define(LSP_ERROR_JSONRPC_RESERVED_ERROR_RANGE_START, -32099).

%% /**
%% * Error code indicating that a server received a notification or
%% * request before the server has received the `initialize` request.
%% */
-define(LSP_ERROR_SERVER_NOT_INITIALIZED,             -32002).
-define(LSP_ERROR_UNKNOWN_ERROR_CODE,                 -32001).

%% /**
%% * This is the start range of JSON RPC reserved error codes.
%% * It doesn't denote a real error code.
%% *
%% * @since 3.16.0
%% */
-define(LSP_ERROR_JSONRPC_RESERVED_ERROR_RANGE_END,   -32000).

%% /**
%% * This is the start range of LSP reserved error codes.
%% * It doesn't denote a real error code.
%% 	 *
%% * @since 3.16.0
%% */
-define(LSP_ERROR_LSP_RESERVED_ERROR_RANGE_START,     -32899).

-define(LSP_ERROR_CONTENT_MODIFIED,                   -32801).
-define(LSP_ERROR_REQUEST_CANCELLED,                  -32800).

%% /**
%% * This is the end range of LSP reserved error codes.
%% * It doesn't denote a real error code.
%% 	 *
%% * @since 3.16.0
%% */
-define(LSP_ERROR_LSP_RESERVED_ERROR_RANGE_END,       -32800).

%%==============================================================================
%% Type Definitions
%%==============================================================================

%% Remember: in map type specs, := means _mandatory_ and => means _optional_
%% We use binary() to mean JSON string

%% Base protocol
-type array() :: list().
-type object() :: map().

-type message() :: #{ jsonrpc := binary() }.
-type params() :: array() | object() | null.

-type requestId() :: integer() | binary().
-type requestMessage() :: #{ jsonrpc := binary()
                           , id := requestId()
                           , method := binary()
                           , params => params()
                           }.

-type responseError() :: #{ code := integer()
                          , message := binary()
                          , data => binary() | number() | boolean() | array() | object() | null
                          }.
-type responseId() :: integer() | binary() | null.
-type responseResult() :: binary() | number() | boolean() | object() | null.
-type responseMessage() :: #{ jsonrpc := binary()
                            , id := responseId()
                            , result => responseResult()
                            , error => responseError()
                            }.

-type notificationMessage() :: #{ method := binary()
                                , params => params()
                                }.

%% Common
-type uri() :: binary().

%% build/initialize
-type buildClientCapabilities() :: #{ languageIds := [binary()] }.
-type initializeBuildParams() :: #{ displayName := binary()
                                  , version := binary()
                                  , bspVersion := binary()
                                  , rootUri := uri()
                                  , capabilities := buildClientCapabilities()
                                  , data => any()
                                  }.
-type compileProvider() :: #{ languageIds := [binary()] }.
-type runProvider() :: #{ languageIds := [binary()] }.
-type testProvider() :: #{ languageIds := [binary()] }.
-type debugProvider() :: #{ languageIds := [binary()] }.
-type buildServerCapabilities() :: #{ compileProvider => compileProvider()
                                    , testProvider => testProvider()
                                    , runProvider => runProvider()
                                    , debugProvider => debugProvider()
                                    , inverseSourcesProvider => boolean()
                                    , dependencySourcesProvider => boolean()
                                    , dependencyModulesProvider => boolean()
                                    , resourcesProvider => boolean()
                                    , canReload => boolean()
                                    , buildTargetChangedProvider => boolean()
                                    }.
-type initializeBuildResult() :: #{ displayName := binary()
                                  , version := binary()
                                  , bspVersion := binary()
                                  , capabilities := buildServerCapabilities()
                                  , data => any()
                                  }.

%% build/initialized
-type initializedBuildParams() :: #{}.

%% workspace/buildTargets
-type workspaceBuildTargetsParams() :: #{}.
-type buildTargetIdentifier() :: #{ uri := uri() }.
-type buildTargetCapabilities() :: #{ canCompile := boolean()
                                    , canTest := boolean()
                                    , canRun := boolean()
                                    }.
-type buildTarget() :: #{ id := buildTargetIdentifier()
                        , displayName => binary()
                        , baseDirectory => uri()
                        , tags => [binary()]
                        , capabilities := buildTargetCapabilities()
                        , languageIds := [binary()]
                        , dependencies := [buildTargetIdentifier()]
                        , dataKind => binary()
                        , data => any()
                        }.
-type workspaceBuildTargetsResult() :: #{ targets => [buildTarget()]}.

%% buildTarget/sources
-type buildTargetSourcesParams() :: #{ targets := [buildTargetIdentifier()] }.
-type sourceItemKind() :: ?SOURCE_ITEM_KIND_FILE
                        | ?SOURCE_ITEM_KIND_DIR.
-type sourceItem() :: #{ uri := uri()
                       , kind := sourceItemKind()
                       , generated := boolean()
                       }.
-type sourcesItem() :: #{ target := buildTargetIdentifier()
                        , sources := [sourceItem()]
                        , roots => [uri()]
                        }.
-type buildTargetSourcesResult() :: #{ items := [sourcesItem()] }.

%% buildTarget/dependencySources
-type dependencySourcesParams() :: #{ targets := [buildTargetIdentifier()] }.
-type dependencySourcesItem() :: #{ target := buildTargetIdentifier()
                                  , sources := [uri()]
                                  }.
-type dependencySourcesResult() :: #{ items := [dependencySourcesItem()] }.

%% buildTarget/compile
-type compileParams() :: #{ targets := [buildTargetIdentifier()]
                          , originId => binary()
                          , arguments => [binary()]
                          }.
-type compileResult() :: #{ originId => binary()
                          , statusCode := integer()
                            %% The spec indicates that dataKind is required, but a comment says
                            %% "If this field is not set, the kind of data is not specified",
                            %% so it seems it is actually optional
                          , dataKind => binary()
                          , data => any()
                          }.


%% buildTarget/test
-type testParams() :: #{ targets := [buildTargetIdentifier()]
                       , originId => binary()
                       , arguments => [binary()]
                       , dataKind => binary()
                       , data => any()
                       }.
-type testResult() :: #{ originId => binary()
                       , statusCode => integer()
                       , dataKind => binary()
                       , data => any()
                       }.

%% rebar3/run
-type rebar3RunParams() :: #{ args := [binary()] }.
-type rebar3RunResult() :: #{}.

