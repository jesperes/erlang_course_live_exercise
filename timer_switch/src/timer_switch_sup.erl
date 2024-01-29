-module(timer_switch_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

child_spec(Mod) ->
  #{ id => Mod
   , start => {Mod, start_link, []}
   }.

init([]) ->
  SupervisorSpec = #{},
  ChildSpecs = [child_spec(timer_hub)],
  {ok, {SupervisorSpec, ChildSpecs}}.
