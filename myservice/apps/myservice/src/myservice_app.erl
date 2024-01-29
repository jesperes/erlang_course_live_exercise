%%%-------------------------------------------------------------------
%% @doc myservice public API
%% @end
%%%-------------------------------------------------------------------

-module(myservice_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(StartType, StartArgs) ->
  logger:reconfigure(),
  ?LOG_INFO(#{string => "Started",
              start_type => StartType,
              start_args => StartArgs}),
  myservice_sup:start_link().

stop(_State) ->
  ok.
