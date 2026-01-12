-module(demo_counter_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_count/0, increment/0, increment/1, reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    count = 0 :: non_neg_integer(),
    version = "0.1.0" :: string()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_count() -> non_neg_integer().
get_count() ->
    gen_server:call(?SERVER, get_count).

-spec increment() -> non_neg_integer().
increment() ->
    increment(1).

-spec increment(pos_integer()) -> non_neg_integer().
increment(N) when is_integer(N), N > 0 ->
    gen_server:call(?SERVER, {increment, N}).

-spec reset() -> ok.
reset() ->
    gen_server:call(?SERVER, reset).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[demo_counter] Starting with count=0~n"),
    {ok, #state{count = 0}}.

handle_call(get_count, _From, #state{count = Count} = State) ->
    {reply, Count, State};

handle_call({increment, N}, _From, #state{count = Count} = State) ->
    NewCount = Count + N,
    {reply, NewCount, State#state{count = NewCount}};

handle_call(reset, _From, State) ->
    io:format("[demo_counter] Counter reset to 0~n"),
    {reply, ok, State#state{count = 0}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Hot code reload - preserve state across upgrades!
%% This is the key feature demonstrated by bc_gitops
code_change(OldVsn, #state{count = Count} = State, _Extra) ->
    io:format("[demo_counter] Hot reload! Old version: ~p, preserving count=~p~n",
              [OldVsn, Count]),
    {ok, State};

%% Handle upgrade from older state format (if any)
code_change(OldVsn, OldState, _Extra) when is_tuple(OldState) ->
    %% Try to extract count from any tuple state
    Count = case OldState of
        {state, C, _} -> C;
        {state, C} -> C;
        _ -> 0
    end,
    io:format("[demo_counter] Hot reload from ~p! Migrated count=~p~n",
              [OldVsn, Count]),
    {ok, #state{count = Count}}.
