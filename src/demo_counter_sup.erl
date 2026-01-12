-module(demo_counter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [
        #{
            id => demo_counter_server,
            start => {demo_counter_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [demo_counter_server]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
