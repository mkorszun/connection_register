%% Copyright
-module(connection_register_server).
-author("mateusz").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
-record(state, {listen_socket}).

init(_Args) ->
    {ok, Port} = application:get_env(connection_register, port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, binary]),
    process_flag(trap_exit, true),
    erlang:send_after(0, self(), accept),
    {ok, #state{listen_socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(accept, #state{listen_socket = ListenSocket} = State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket, 2000),
    gen_tcp:controlling_process(Socket, self()),
    spawn_link(
        fun() ->
            {ok, {Address, Port}} = inet:peername(Socket),
            io:format("Connection accepted, updating counter: ~p ~p", [Address, Port]),
            ok = connection_register_counter:add(Address, Port)
        end
    ),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    spawn_link(
        fun() ->
            {ok, {Address, Port}} = inet:peername(Socket),
            io:format("Connection closed, updating counter: ~p ~p", [Address, Port]),
            ok = connection_register_counter:remove(Address, Port)
        end
    ),
    {noreply, State};

handle_info({'EXIT', _PID, Reason}, State) ->
    io:format("Failed to update counter: ~p", [Reason]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


