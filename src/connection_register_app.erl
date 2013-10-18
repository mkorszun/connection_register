-module(connection_register_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2, stop/1]).

%% ===================================================================
%% Application api
%% ===================================================================

start() ->
    ok = mnesia:start().

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Nodes} = application:get_env(connection_register, nodes),
    lists:foreach(fun(Node) -> pong = net_adm:ping(Node) end, Nodes),
    ok = connection_register_counter:init(Nodes),
    connection_register_sup:start_link().

stop(_State) ->
    ok.

