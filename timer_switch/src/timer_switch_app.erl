%%%-------------------------------------------------------------------
%% @doc timer_switch public API
%% @end
%%%-------------------------------------------------------------------

-module(timer_switch_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    timer_switch_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
