%%% @doc Connection register api

-module(connection_register).

-export([start/0, join/0, init_cluster/0, init_cluster/1, list_connections/0]).

%% ===================================================================
%% Api
%% ===================================================================

%% @doc Start mnesia, tcp server and connection supervisor
start() ->
    ok = mnesia:start(),
    ok = application:start(connection_register).

join() ->
    {ok, ContactNode} = application:get_env(connection_register, conntact_node),
    pong = net_adm:ping(ContactNode).

%% @doc Setup cluster on default node list specified in config
init_cluster() ->
    {ok, Nodes} = application:get_env(connection_register, nodes),
    init_cluster(Nodes).

%% @doc Setup cluster on specified node list or all visible nodes
init_cluster(all_visible_nodes) ->
    init_cluster([node()|nodes()]);
init_cluster(Nodes) ->
    lists:foreach(fun(Node) -> pong = net_adm:ping(Node) end, Nodes),
    {atomic, ok} = tcp_connection_counter:init(Nodes).

%% @doc List active connections
list_connections() ->
    tcp_connection_counter:list().

%% ===================================================================
%% ===================================================================
%% ===================================================================

