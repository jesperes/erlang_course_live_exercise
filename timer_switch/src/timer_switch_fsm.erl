-module(timer_switch_fsm).

-behaviour(gen_statem).

%% API
-export([ start_link/1
        , on/1
        , off/1
        , countdown/2
        ]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

start_link(Name) ->
  gen_statem:start_link({local, Name}, ?MODULE, [], []).

on(Name) ->
  gen_statem:call(Name, on).

off(Name) ->
  gen_statem:call(Name, off).

countdown(Name, Time) ->
  gen_statem:cast(Name, {countdown, Time}).

callback_mode() ->
  handle_event_function.

init([]) ->
  process_flag(trap_exit, true),
  io:format("Starting fsm: ~p~n", [self()]),
  {ok, off, 0}.

handle_event(cast, {countdown, Time}, State, Data) ->
  io:format("Countdown: ~p~n", [Time]),
  {next_state, State, Data, {timeout, Time, countdown}};
handle_event(timeout, countdown, State, Data) ->
  NewState =
    case State of
      off -> on;
      on -> off
    end,
  {next_state, NewState, Data + 1};
handle_event({call, From}, on = _NewEvent, off = _OldState, Data) ->
  {next_state, on, Data + 1, {reply, From, ok}};
handle_event({call, From}, off = _NewEvent, on = _OldState, Data) ->
  {next_state, off, Data + 1, {reply, From, ok}};
handle_event({call, From}, State, State, Data) ->
  %% Ignore on->on and off->off
  {next_state, State, Data, {reply, From, ok}};
handle_event({call, From}, NewState, OldState, Data) ->
  {next_state, OldState, Data + 1,
   {reply, From, {error, invalid_state_transition, OldState, NewState}}}.

terminate(_Reason, _State, _Data) ->
  void.
