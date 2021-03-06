%%% @doc Connection register application

-module(connection_register_app).
-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    connection_register_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% ===================================================================
%% ===================================================================

