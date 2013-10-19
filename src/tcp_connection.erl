%%% @doc TCP connection process / controls socket lifecycle

-module(tcp_connection).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================================================================
%% ===================================================================
%% ===================================================================

-record(state, {socket}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([Socket]) ->
    erlang:send_after(0, self(), tcp_opened),
    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% Called after connection has been accepted to update connection register
handle_info(tcp_opened, #state{socket = Socket} = State) ->
    {ok, {Address, Port}} = inet:peername(Socket),
    io:format("~nConnection accepted, adding to register: ~p:~p", [Address, Port]),
    ok = tcp_connection_counter:add(self(), Address, Port),
    {noreply, State};

%% Called whenever connection terminates - triggers process termination
handle_info({tcp_closed, _}, State) ->
    {stop, tcp_closed, State};

%% Log arrived data
handle_info(Data, State) ->
    io:format("~nData arrived: ~p", [Data]),
    {noreply, State}.

%% Removes connection socket address from register
terminate(Reason, #state{socket = Socket}) ->
    {Address, Port} = tcp_connection_counter:read(self()),
    io:format("~nConnection terminating: ~p, removing from: ~p:~p", [Reason, Address, Port]),
    ok = tcp_connection_counter:remove(self()),
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% ===================================================================
%% ===================================================================