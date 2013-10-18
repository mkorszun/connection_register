%% Copyright
-module(connection_register_counter).
-author("mateusz").

-export([init/1, add/2, remove/2, list/0]).

-record(connection, {socket_address}).

init(Nodes) ->
    mnesia:create_table(connection, [{attributes, record_info(fields, connection)}, {disc_copies, Nodes}]).

add(Address, Port) ->
    mnesia:dirty_write(#connection{socket_address = {Address, Port}}).

remove(Address, Port) ->
    mnesia:dirty_delete_object(#connection{socket_address = {Address, Port}}).

list() ->
    traverse_table_and_show(connection).

%% ===================================================================
%% Internal functions
%% ===================================================================

traverse_table_and_show(Table_name) ->
    Iterator = fun(#connection{socket_address = {Address, Port}}, _) ->
        io:format("~p:~p~n", [Address, Port]),
        []
    end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator, [], Table_name);
        false ->
            Exec = fun({Fun, Tab}) -> mnesia:foldl(Fun, [], Tab) end,
            mnesia:activity(transaction, Exec, [{Iterator, Table_name}], mnesia_frag)
    end.
