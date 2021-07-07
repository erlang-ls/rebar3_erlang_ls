-module(sample_app_ping_pong).
-behaviour(gen_server).

-export([ start_link/0
        , ping/1
        , init/1
        , handle_call/3
        , handle_cast/2
        ]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ping(term()) -> ok.
ping(Message) ->
  gen_server:call(?SERVER, {ping, Message}).

-spec init([]) -> {ok, #{}}.
init([]) ->
  {ok, #{}}.

-spec handle_call({ping, term()}, {pid(), term()}, map()) -> {reply, ok, map()}.
handle_call({ping, Message}, {Sender, _Tag} = _From, State) ->
  Sender ! {Message, self()},
  {reply, ok, State}.

-spec handle_cast(term(), map()) -> no_return().
handle_cast(Request, State) ->
  erlang:error(badarg, [Request, State]).

