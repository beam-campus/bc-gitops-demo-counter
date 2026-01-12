-module(demo_counter_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(demo_counter, http_port, 8080),

    %% Set up Cowboy routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/count", demo_counter_http, []},
            {"/increment", demo_counter_http, []},
            {"/reset", demo_counter_http, []},
            {"/health", demo_counter_http, []}
        ]}
    ]),

    %% Start Cowboy
    {ok, _} = cowboy:start_clear(
        demo_counter_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("demo_counter HTTP API listening on port ~p~n", [Port]),

    demo_counter_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(demo_counter_http_listener),
    ok.
