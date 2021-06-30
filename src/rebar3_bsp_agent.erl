-module(rebar3_bsp_agent).
-behaviour(gen_server).

-export([ start_link/1
        , stop/0
        , post_response/1
        , notify/2
        , request/2
        , post_request/2
        , receive_response/2
        , wait_response/2
        , check_response/2
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-include("rebar3_bsp.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: #{ rebar3_state := rebar_state:t()
                  , is_initialized := boolean()
                  , is_shutdown := boolean()
                  }.

-type methodName() :: atom() | binary() | list().
-type methodParams() :: map() | null.
-type pendingRequest() :: term().

-export_type([state/0]).

%%==============================================================================
%% API
%%==============================================================================

-spec start_link(rebar_state:t()) -> {ok, pid()}.
start_link(R3State) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, R3State, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec post_response(map()) -> ok.
post_response(Response) ->
  gen_server:cast(?SERVER, {response, Response}).

-spec notify(methodName(), methodParams()) -> ok.
notify(Method, Params) ->
  gen_server:cast(?SERVER, {notify, rebar3_bsp_util:to_atom(Method), Params}).

-spec request(methodName(), methodParams()) -> term().
request(Method, Params) ->
  gen_server:call(?SERVER, {request, rebar3_bsp_util:to_atom(Method), Params}).

-spec post_request(methodName(), methodParams()) -> pendingRequest().
post_request(Method, Params) ->
  ?GEN_SERVER_SEND_REQUEST(?SERVER, {request, rebar3_bsp_util:to_atom(Method), Params}).

-spec receive_response(pendingRequest(), timeout()) -> {reply, term()} | timeout | {error, {term(), term()}}.
receive_response(Id, Timeout) ->
  ?GEN_SERVER_RECEIVE_RESPONSE(Id, Timeout).

-spec wait_response(pendingRequest(), timeout()) -> {reply, term()} | timeout | {error, {term(), term()}}.
wait_response(Id, Timeout) ->
  ?GEN_SERVER_WAIT_RESPONSE(Id, Timeout).

-spec check_response(term(), pendingRequest()) -> {reply, term()} | no_reply | {error, {term(), term()}}.
check_response(Msg, Id) ->
  ?GEN_SERVER_CHECK_RESPONSE(Msg, Id).

%%==============================================================================
%% gen_server Callbacks
%%==============================================================================
-spec init(rebar_state:t()) -> {ok, state()}.
init(R3State) ->
  os:putenv("REBAR_COLOR", "none"),
  application:set_env(rebar, color_intensity, none),
  {ok, #{ rebar3_state => R3State
        , is_initialized => false
        , is_shutdown => false
        }}.

-spec handle_call({request, atom(), methodParams()}, term(), state()) -> {reply, term(), state()}.
handle_call({request, Method, Params}, _From, State) ->
  case try_dispatch(Method, Params, State) of
    {response, Result, NewState} ->
      {reply, {ok, Result}, NewState};
    {noresponse, NewState} ->
      ?LOG_ALERT("No response to request [request=~p] ", [{Method, Params}]),
      {reply, {ok, null}, NewState};
    {error, Error, NewState} ->
      {reply, {error, Error}, NewState}
  end.

-spec handle_cast({notify, atom(), methodParams()}, state()) -> {noreply, state()};
                 ({exit, term()}, state()) -> {stop, term(), state()}.
handle_cast({notify, Method, Params}, State) ->
  case try_dispatch(Method, Params, State) of
    {response, Result, NewState} ->
      ?LOG_ALERT("Unexpected response to notification [notification=~p] [response=~p]",
                 [{Method, Params}, Result]),
      {noreply, NewState};
    {noresponse, NewState} ->
      {noreply, NewState};
    {error, Error, NewState} ->
      ?LOG_CRITICAL("Error processing notification [notification=~p] [error=~p]", [{Method, Params}, Error]),
      {noreply, NewState}
  end;
handle_cast({exit, 0}, State) ->
  {stop, normal, State};
handle_cast({exit, _ExitCode} = Reason, State) ->
  {stop, {shutdown, Reason}, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec try_dispatch(atom(), methodParams(), state()) ->
        {noresponse, state()} |
        {response, term(), state()} |
        {error, term(), state()}.
try_dispatch(Method, Params, State) ->
  try
    case rebar3_bsp_methods:Method(Params, State) of
      {exit, ExitCode, NewState} ->
        post_exit(self(), ExitCode),
        {noresponse, NewState};
      Result ->
        Result
    end
  catch
    Class:Error:Stacktrace ->
      {Code, Msg} = case erlang:function_exported(rebar3_bsp_methods, Method, 2) of
                      false ->
                        {?LSP_ERROR_METHOD_NOT_FOUND, io_lib:format("Method not found. [method=~p]", [Method])};
                      true ->
                        {?LSP_ERROR_INTERNAL_ERROR, io_lib:format("~p:~p ~p", [Class, Error, Stacktrace])}
                    end,
      {error, #{ code => Code, message => rebar3_bsp_util:to_binary(Msg) }, State}
  end.

-spec post_exit(pid(), term()) -> ok.
post_exit(Pid, ExitCode) ->
  gen_server:cast(Pid, {exit, ExitCode}).

