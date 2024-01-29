-module(timer_hub).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([ start_link/0
        , stop/0
        , connect/1
        , disconnect/1
        , on/1
        , off/1
        , countdown/2
        , i/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

connect(Name) ->
  gen_server:call(?SERVER, {connect, Name}).

disconnect(Name) ->
  gen_server:call(?SERVER, {disconnect, Name}).

on(Name) ->
  gen_server:call(?SERVER, {on, Name}).

off(Name) ->
  gen_server:call(?SERVER, {off, Name}).

stop() ->
  gen_server:stop(?SERVER).

countdown(Name, Msecs) ->
  gen_server:call(?SERVER, {countdown, Name, Msecs}).

i() ->
  gen_server:call(?SERVER, i).

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call(i, _From, State) ->
  {links, Links} = process_info(self(), links),
  Switches =
    lists:filtermap(
      fun(Pid) ->
          {registered_name, Name} = process_info(Pid, registered_name),
          case Name of
            timer_switch_sup -> false;
            _ -> {true, Name}
          end
      end, Links),

  Str =
    if length(Switches) == 0 ->
        "No timer switches defined.";
       true ->
        lists:map(
          fun(Name) ->
              {FsmState, _Data} = sys:get_state(Name),
              io_lib:format("Timer switch:~n"
                            "\tName: ~ts~n"
                            "\tPid: ~w~n"
                            "\tState: ~ts~n",
                            [Name, whereis(Name), FsmState])
          end, Switches)
    end,

  io:format("~ts~n", [Str]),
  {reply, ok, State};
handle_call({disconnect, Name}, _From, State) ->
  case whereis(Name) of
    undefined ->
      {reply, {no_such_switch, Name}, State};
    Pid ->
      exit(Pid, normal),
      {reply, ok, State}
  end;
handle_call({connect, Name}, _From, State) ->
  case timer_switch_fsm:start_link(Name) of
    {error, _} = Error ->
      {reply, Error, State};
    {ok, _Pid} ->
      {reply, ok, State}
  end;
handle_call({Action, Name}, _From, State) when Action =:= on orelse
                                               Action =:= off ->
  try
    Result = timer_switch_fsm:Action(Name),
    {reply, Result, State}
  catch exit:{noproc, _} ->
      {reply, {no_such_switch, Name}, State}
  end;
handle_call({countdown, Name, Time}, _From, State) ->
  try
    Result = timer_switch_fsm:countdown(Name, Time),
    {reply, Result, State}
  catch exit:{noproc, _} ->
      {reply, {no_such_switch, Name}, State}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};
handle_info(Request, State) ->
  ?LOG_ERROR("Unexpected info event: ~p", [Request]),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

format_status(_Opt, Status) ->
  Status.
