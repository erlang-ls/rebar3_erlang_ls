-module(rebar3_bsp_echo_port).
-export([ start_link/0
        , stop/1
        , set_endpoints/3
        , init/1
        , system_terminate/4
        , system_continue/3]).

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

stop(Pid) ->
    proc_lib:stop(Pid).

set_endpoints(Pid, A, B) when is_pid(A), is_pid(B) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, {set_endpoints, {A, B}}},
    receive
        {Ref, ok} ->
            ok
    end.

init(Parent) ->
    proc_lib:init_ack({ok, self()}),
    Deb = sys:debug_options([]),
    loop(undefined, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, Endpoints) ->
    terminate(Endpoints, Reason).

system_continue(Parent, Deb, Endpoints) ->
    loop(Endpoints, Parent, Deb).

loop(Endpoints, Parent, Deb) ->
    receive
        {From, Ref, set_endpoints, {A, B}} when Endpoints =:= undefined ->
            From ! {Ref, ok},
            loop({A, B}, Parent, Deb);
        {From, {command, Data}} when Endpoints =/= undefined ->
            handle_data(Endpoints, From, Data),
            loop(Endpoints, Parent, Deb);
        {From, close} when Endpoints =/= undefined ->
            handle_close(Endpoints, From),
            loop(Endpoints, Parent, Deb);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, Endpoints)
    end.

handle_data(Endpoints, From, Data) ->
    {_Sender, Receiver} = get_sender_receiver(Endpoints, From),
    Receiver ! {self(), {data, Data}}.

handle_close(Endpoints, From) ->
    {Sender, _Receiver} = get_sender_receiver(Endpoints, From),
    Sender ! {self(), closed},
    terminate(Endpoints, closed).

terminate(Endpoints, Reason) ->
    case Endpoints of
        {A, B} ->
            send_exit(A, Reason),
            send_exit(B, Reason);
        _ ->
            ok
    end,
    exit(Reason).

send_exit(To, Reason) ->
    To ! {'EXIT', self(), Reason}.

get_sender_receiver({A, B}, From) ->
    case From of
        A ->
            {A, B};
        B ->
            {B, A}
    end.
