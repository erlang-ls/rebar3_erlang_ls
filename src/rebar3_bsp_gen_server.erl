-module(rebar3_bsp_gen_server).

-include("rebar3_bsp.hrl").

-ifndef(HAVE_GEN_SERVER_SEND_REQUEST).

-export([ send_request/2
        , receive_response/2
        , wait_response/2
        , check_response/2
        ]).

%%==============================================================================
%% Compat Stuff
%% Since the build server can take arbitrary amounts of time to process things
%% we really would like to use gen_server:send_request et co that are added in
%% OTP 23, but we also need to support older OTP versions for now - implement
%% rudimentary scaffolding to fake the new functionality.
%% Remove whenever only OTP >= 23 is supported.
%%==============================================================================
-type name() :: atom().
-type global_name() :: term().
-type via_name() :: term().
-type server_ref() :: name() | {name(), node()} | {global, global_name()} | {via, module(), via_name()} | pid().
-type request_id() :: {pid(), reference(), reference(), server_ref()}.


-spec send_request(server_ref(), term()) -> request_id().
send_request(ServerRef, Request) ->
  Self = self(),
  Ref = erlang:make_ref(),
  F = fun() ->
          Result = gen_server:call(ServerRef, Request, infinity),
          try Self ! {Ref, Result} catch _:_ -> ok end
      end,
  {Pid, Mon} = erlang:spawn_monitor(F),
  {Pid, Mon, Ref, ServerRef}.

-spec receive_response(request_id(), timeout()) ->
        {reply, term()} | timeout | {error, {term(), term()}}.
receive_response(RequestId, Timeout) ->
  case wait_response(RequestId, Timeout) of
    timeout ->
      {Pid, Mon, Ref, ServerRef} = RequestId,
      erlang:exit(Pid, kill),
      erlang:demonitor(Mon, [flush]),
      receive
        {Ref, Result} ->
          {reply, Result}
      after 0 ->
          timeout
      end;
    Result ->
      Result
  end.

-spec wait_response(any(), timeout()) ->
        {reply, any()} |
        timeout |
        {error, {any(), server_ref()}}.
wait_response({_Pid, Mon, Ref, ServerRef}, Timeout) ->
  receive
    {Ref, Result} ->
      erlang:demonitor(Mon, [flush]),
      {reply, Result};
    {'DOWN', Mon, _Type, _Object, Info} ->
      {error, {Info, ServerRef}}
  after Timeout ->
      timeout
  end.

-spec check_response(any(), any()) ->
        {reply, any()} |
        no_reply |
        {error, {any(), server_ref()}}.
check_response(Msg, {_Pid, Mon, Ref, ServerRef}) ->
  case Msg of
    {Ref, Result} ->
      erlang:demonitor(Mon, [flush]),
      {reply, Result};
    {'DOWN', Mon, _Type, _Object, Info} ->
      {error, {Info, ServerRef}};
    _ ->
      no_reply
  end.

-endif.

