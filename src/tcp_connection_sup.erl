%%% @doc Supervisor for established tcp connections

-module(tcp_connection_sup).
-behaviour(supervisor).

-export([start_link/0, start_connection/1]).
-export([init/1]).

%% ===================================================================
%% Macros
%% ===================================================================

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_connection(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(tcp_connection, worker)]}}.

%% ===================================================================
%% ===================================================================
%% ===================================================================