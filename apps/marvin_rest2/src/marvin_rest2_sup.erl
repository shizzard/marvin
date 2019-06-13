-module(marvin_rest2_sup).
-behaviour(supervisor).

-include("marvin_rest2.hrl").
-include_lib("marvin_helper/include/marvin_specs_supervisor.hrl").

-export([start_link/0, init/1]).
-export([]).



%% Interface



-spec start_link() ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {
        #{
            strategy => one_for_one,
            intensity => 5,
            period => 10
        },
        [
            #{
                id => {marvin_rest2_queue_sup, RateLimitGroup},
                start => {marvin_rest2_queue_sup, start_link, [RateLimitGroup]},
                restart => permanent, shutdown => 5000, type => supervisor,
                modules => [marvin_rest2_queue_sup]
            } || RateLimitGroup <- [
                ?ratelimit_group_guild,
                ?ratelimit_group_channel,
                ?ratelimit_group_webhook
            ]
        ]
    }}.



%% Internals
