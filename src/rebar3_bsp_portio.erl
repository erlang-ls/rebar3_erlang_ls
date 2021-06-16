-module(rebar3_bsp_portio).
-behavior(gen_server).

-export([ start_link/0
        , start_link/1
        , stop/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        , terminate/2
        ]).

-define(IO_FDS_ENV_VARIABLE, "REBAR3_BSP_IO_FDS").
-define(DEFAULT_IO_FDS, "0 1 2").

-type state() :: #{ fds => {integer(), integer(), integer()},
                    port => port() | undefined,
                    data => binary() }.

-spec start_link() -> {ok, pid()}.
start_link() ->
    Fds = find_io_fds(),
    start_link(Fds).

-spec start_link({integer(), integer(), integer()}) -> {ok, pid()}.
start_link({InFd, OutFd, ErrFd}) ->
    gen_server:start_link(?MODULE, {InFd, OutFd, ErrFd}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec init({integer(), integer()}) -> {ok, state()}.
init({InFd, OutFd, ErrFd}) ->
    case {InFd, OutFd} of
        {0, 1} ->
            %% We're taking over stdio, let's disable stdio loggers to try to avoid contaminating stdout
            [ maybe_remove_logger_handler(H) || H <- logger:get_handler_ids() ];
        _ ->
            ok
    end,
    Port = erlang:open_port({fd, InFd, OutFd}, [binary]),
    {ok, #{ fds => {InFd, OutFd, ErrFd}
          , port => Port
          , data => <<>> }}.

-spec maybe_remove_logger_handler(atom()) -> ok.
maybe_remove_logger_handler(HandlerId) ->
    case logger:get_handler_config(HandlerId) of
        {ok, #{module := logger_std_h, config := #{type := standard_io}}} ->
            logger:remove_handler(HandlerId),
            ok;
        _ ->
            ok
    end.

-spec handle_call(term(), term(), state()) -> ok.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, _State) ->
    error(badarg).

-spec handle_cast(term(), state()) -> no_return().
handle_cast(_Request, _State) ->
    error(badarg).

-spec handle_info(term(), state()) -> {noreply, state(), {continue, term()}} | {stop, term(), state()}.
handle_info({Port, {data, NewData}}, #{port := Port, data := OldData} = State) ->
    AllData = <<OldData/binary, NewData/binary>>,
    case rebar3_bsp_jsonrpc:decode_packets(AllData) of
        {ok, Messages, RestData} ->
            {noreply, State#{data => RestData}, {continue, {dispatch_messages, Messages}}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_info({'EXIT', Port, _Reason} = ExitReason, #{port := Port} = State) ->
    {stop, ExitReason, State#{port => undefined}}.

-spec handle_continue(term(), state()) -> {noreply, state()} | {noreply, state(), {continue, term()}} | {stop, term(), state()}.
handle_continue({dispatch_messages, []}, State) ->
    {noreply, State};
handle_continue({dispatch_messages, [Message|Messages]}, #{ port := Port } = State) ->
    case rebar3_bsp_agent:handle_message(Message) of
        ok ->
            ok;
        {response, Response} ->
            true = port_command(Port, Response)
    end,
    {noreply, State, {continue, {dispatch_messages, Messages}}}.

-spec terminate(term(), state()) -> term().
terminate(_Reason, #{ port := undefined } = _State) ->
    ok;
terminate(_Reason, #{ port := Port } = _State) ->
    true = port_close(Port),
    ok.

-spec find_io_fds() -> {integer(), integer(), integer()}.
find_io_fds() ->
    IoFdString = os:getenv(?IO_FDS_ENV_VARIABLE, ?DEFAULT_IO_FDS),
    {ok, [InFd, OutFd, ErrFd], _Garbage} = io_lib:fread("~d ~d ~d", IoFdString),
    {InFd, OutFd, ErrFd}.
