%%% @doc TCP connection counter api

-module(tcp_connection_counter).

-export([init/1, add/3, read/1, remove/1, list/0]).

%% ===================================================================
%% ===================================================================
%% ===================================================================

-record(conn, {pid, socket_address}).

%% ===================================================================
%% Connection counter API
%% ===================================================================

%% @doc Sets up mnesia cluster on given nodes
init(Nodes) ->
    mnesia:change_config(extra_db_nodes, Nodes),
    mnesia:create_table(conn, [{attributes, record_info(fields, conn)}, {ram_copies, Nodes}]).

%% @doc Store in mnesia cluster connection attributes identified by given process
add(Pid, Address, Port) ->
    mnesia:dirty_write(#conn{pid = Pid, socket_address = {Address, Port}}).

%% @doc Read from mnesia cluster connection attributes identified by given process
read(Pid) ->
    [Connection] = mnesia:dirty_read(conn, Pid),
    Connection#conn.socket_address.

%% @doc Remove from mnesia cluster connection attributes identified by given process
remove(Pid) ->
    mnesia:dirty_delete({conn, Pid}).

%% @doc Print all active connections with corresponding connection processes
list() ->
    traverse_table_and_show(conn).

%% ===================================================================
%% Internal functions
%% ===================================================================

traverse_table_and_show(Table) ->
    case mnesia:is_transaction() of
        true ->
            mnesia:foldl(to_str(), [], Table);
        false ->
            Exec = fun({Fun, Tab}) -> mnesia:foldl(Fun, [], Tab) end,
            mnesia:activity(transaction, Exec, [{to_str(), Table}], mnesia_frag)
    end.

to_str() ->
    fun(#conn{pid = Pid, socket_address = {Address, Port}}, _) ->
        io:format("~p ==>> ~p:~p~n", [Pid, Address, Port])
    end.

%% ===================================================================
%% ===================================================================
%% ===================================================================
