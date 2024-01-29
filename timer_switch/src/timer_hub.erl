-module(timer_hub).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , stop/0
        , connect/1
        , disconnect/1
        , on/1
        , off/1
        , countdown/2
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

init([]) ->
  process_flag(trap_exit, true),
  io:format("timer_hub starting!~n", []),
  {ok, #state{}}.

handle_call({disconnect, Name}, _From, State) ->
  case whereis(Name) of
    undefined -> {reply, {no_such_switch, Name}, State};
    Pid ->
      exit(Pid, normal),
      {reply, ok, State}
  end;
handle_call({connect, Name}, _From, State) ->-
  case timer_switch_fsm:start_link(Name) of
    {error, _} = Error ->
      {reply, Error, State};
    {ok, Pid} ->
      io:format("Started new timer switch ~p~n", [Pid]),
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

handle_cast(Request, State) ->
  io:format("Undefined cast: ~p~n", [Request]),
  {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State}.

%handle_info(Info, State) ->
%  io:format("Info: ~p~n", [Info]),
%  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("timer_hub terminating~n", []),
  ok.

format_status(_Opt, Status) ->
  Status.