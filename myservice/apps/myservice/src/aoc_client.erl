-module(aoc_client).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([ start_link/0
        , get_leaderboard/0
        , get_leaderboard/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

-record(state, { leaderboard
               , age
               }).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

aoc_session_token() ->
  {ok, Filename} = application:get_env(myservice, aoc_session_cookie_file),
  {ok, Bin} = file:read_file(Filename),
  string:trim(Bin).

get_leaderboard() ->
  get_leaderboard(2023, 2939112). %% Klarna's 2023 leaderboard

get_leaderboard(Year, Id) ->
  gen_server:call(?SERVER, {get_leaderboard, Year, Id}).

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call({get_leaderboard, Year, Id}, _From, State) ->
  CookieHeader = io_lib:format("session=~s", [aoc_session_token()]),
  Url = io_lib:format("https://adventofcode.com/~w/leaderboard/private/view/~w.json", [Year, Id]),
  ?LOG_INFO(#{string => "Requesting leaderboard",
              year => Year,
              id => Id,
              url => Url
             }),
  Headers = [ {"cookie", CookieHeader}
            , {"Content-Type", "application/json; charset=UTF-8"}
            ],
  Request = {Url, Headers},
  HttpOptions = [],
  Options = [{body_format, binary}],
  Result =
    case httpc:request(get, Request, HttpOptions, Options) of
      {ok, {{_, 200, _}, _, Body}} ->
        {ok, Body};
      {ok, {{_, 404, _}, _, _}} ->
        {error, not_found}
    end,
  {reply, Result, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
