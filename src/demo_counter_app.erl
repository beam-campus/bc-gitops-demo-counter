-module(demo_counter_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(demo_counter, http_port, 8082),
    io:format("[demo_counter] Starting with http_port=~p~n", [Port]),

    %% Set up Cowboy routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/count", demo_counter_http, []},
            {"/increment", demo_counter_http, []},
            {"/reset", demo_counter_http, []},
            {"/health", demo_counter_http, []},
            {"/info", demo_counter_http, []}
        ]}
    ]),

    %% Start Cowboy
    case cowboy:start_clear(
        demo_counter_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ) of
        {ok, _} ->
            io:format("[demo_counter] HTTP API listening on port ~p~n", [Port]),
            demo_counter_sup:start_link();
        {error, Reason} ->
            io:format("[demo_counter] Failed to start on port ~p: ~p~n", [Port, Reason]),
            {error, Reason}
    end.

stop(_State) ->
    cowboy:stop_listener(demo_counter_http_listener),
    ok.
