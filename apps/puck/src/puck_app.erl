%%%-------------------------------------------------------------------
%% @doc puck public API
%% @end
%%%-------------------------------------------------------------------

-module(puck_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    puck_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
