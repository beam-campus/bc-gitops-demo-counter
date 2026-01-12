-module(demo_counter_http).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {StatusCode, Body} = handle_request(Method, Path),
    Req = cowboy_req:reply(
        StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req0
    ),
    {ok, Req, State}.

%% GET /count - Get current count
handle_request(<<"GET">>, <<"/count">>) ->
    Count = demo_counter_server:get_count(),
    {200, #{count => Count}};

%% POST /increment - Increment by 1
handle_request(<<"POST">>, <<"/increment">>) ->
    NewCount = demo_counter_server:increment(),
    {200, #{count => NewCount, action => <<"incremented">>}};

%% POST /reset - Reset to 0
handle_request(<<"POST">>, <<"/reset">>) ->
    ok = demo_counter_server:reset(),
    {200, #{count => 0, action => <<"reset">>}};

%% GET /health - Health check
handle_request(<<"GET">>, <<"/health">>) ->
    Count = demo_counter_server:get_count(),
    {200, #{
        status => <<"healthy">>,
        app => <<"demo_counter">>,
        count => Count
    }};

%% GET /version - Version info
handle_request(<<"GET">>, <<"/version">>) ->
    {ok, Vsn} = application:get_key(demo_counter, vsn),
    {200, #{
        app => <<"demo_counter">>,
        version => list_to_binary(Vsn),
        description => <<"Demo counter for bc_gitops hot reload">>
    }};

%% Method not allowed
handle_request(Method, Path) ->
    {405, #{
        error => <<"method_not_allowed">>,
        method => Method,
        path => Path
    }}.
