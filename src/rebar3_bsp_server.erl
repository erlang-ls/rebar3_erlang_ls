-module(rebar3_bsp_server).
-behavior(gen_server).

-include("rebar3_bsp.hrl").

%% API
-export([ start_link/1
        , stop/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        ]).

-define(IO_FDS_ENV_VARIABLE, "REBAR3_BSP_IO_FDS").
-define(DEFAULT_IO_FDS, "0 1 2").
-define(SERVER, ?MODULE).

-type state() :: #{ rebar3_state => rebar3_state:t()
                  , server_state => server_state()
                  , ioport => port() | undefined
                  , errfd => integer() | undefined 
                  , buffer => binary()
                  }.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link( {rebar3_state:t()}
                | {rebar3_state:t(), {fd, integer(), integer()}, integer()}
                | {rebar3_state:t(), {port, pid()}, integer()}
                ) -> {ok, pid()}.
start_link(InitArgs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitArgs, []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

handle_request(Request) ->
    gen_server:call(?SERVER, {handle_request, Request}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init( {rebar3_state:t()}
          | {rebar3_state:t(), {fd, integer(), integer()}, integer()}
          | {rebar3_state:t(), {port, pid()}, integer}
          ) -> {ok, state()}.
init({R3State}) ->
    {InFd, OutFd, ErrFd} = find_io_fds(),
    init({R3State, {fd, InFd, OutFd}, ErrFd});
init({R3State, {fd, InFd, OutFd}, ErrFd}) ->
    Port = erlang:open_port({fd, InFd, OutFd}, [binary]),
    init({R3State, {port, Port}, ErrFd});
init({R3State, {port, Port}, ErrFd}) ->
    {ok, #{ rebar3_state => R3State
          , server_state => #{ is_initialized => false
                             , is_shutdown => false
                             }
          , ioport => Port
          , errfd => ErrFd
          , buffer => <<>>}}.

-spec handle_call(stop, pid(), state()) -> {stop, normal, ok, state()}.
handle_call({handle_request, Request}, _From, State) ->
    {ok, Result} = 
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec handle_cast(any(), state()) -> no_return().
handle_cast(_Request, _State) ->
    error(badarg).

-spec handle_info({pid(), {data, binary()}}, state()) -> {noreply, state(), {continue, decode}}.
handle_info({Port, {data, Data}}, #{port := Port, buffer := Buffer} = State) ->
    {noreply, State#{buffer => <<Buffer/binary, Data/binary>>}, {continue, decode}}.

-spec handle_continue(decode, state()) -> {noreply, state(), {continue, {dispatch_messages, [map()]}}}
                                          | {stop, any(), state()}
                   ; ({dispatch_messages, []}, state()) -> {noreply, state()}
                   ; ({dispatch_messages, [map()]}, state()) -> {noreply, state(), {continue, {dispatch_messages, [map()]}}}.
handle_continue(decode, #{buffer := Buffer} = State) ->
    case rebar3_bsp_jsonrpc:decode_packets(Buffer) of
        {ok, Messages, RestData} ->
            {noreply, State#{buffer => RestData}, {continue, {dispatch_messages, Messages}}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_continue({dispatch_messages, []}, State) ->
    {noreply, State};
handle_continue({dispatch_messages, [M|Ms]}, State) ->
    {noreply, handle_message(M, State), {continue, {dispatch_messages, Ms}}}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec find_io_fds() -> {integer(), integer(), integer()}.
find_io_fds() ->
    IoFdString = os:getenv(?IO_FDS_ENV_VARIABLE, ?DEFAULT_IO_FDS),
    {ok, [InFd, OutFd, ErrFd], _Garbage} = io_lib:fread("~d ~d ~d", IoFdString),
    {InFd, OutFd, ErrFd}.

-spec handle_message(map(), state()) -> state().
handle_message(#{id := Id, method := Method, params := Params},
               #{port := Port, server_state := ServerState, rebar3_state := R3State} = State) ->
    %% message is a request
    {Result, NewServerState, NewR3State} = rebar3_bsp_methods:Method(Params, ServerState, R3State),
    Response = rebar3_bsp_protocol:response(Id, Result),
    erlang:port_command(Port, Response),
    State#{server_state => NewServerState, rebar3_state => NewR3State};
handle_message(#{id := _Id, result := _Result, error:= _Error}, State) ->
    %% message is a response -- todo
    State;
handle_message(#{method := Method, params := Params},
               #{server_state := ServerState, rebar3_state := R3State} = State) ->
    %% message is a notification
    {ok, NewServerState, NewR3State} = rebar3_bsp_methods:Method(Params, ServerState, R3State),
    State#{server_state => NewServerState, rebar3_state => NewR3State}.

