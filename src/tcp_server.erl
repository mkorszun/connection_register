%%% @doc TCP server process

-module(tcp_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================================================================
%% ===================================================================
%% ===================================================================

-record(state, {listen_socket}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

%% Create listening socket and starts accept loop
init(_) ->
    {ok, Port} = application:get_env(connection_register, port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, {packet, 0}, binary]),
    erlang:send_after(0, self(), accept),
    {ok, #state{listen_socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% Accept loop
handle_info(accept, #state{listen_socket = ListenSocket} = State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, Pid} = tcp_connection_sup:start_connection(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    erlang:send_after(0, self(), accept),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{listen_socket = ListenSocket}) ->
    io:format("TCP server termination: ~p", [Reason]),
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


