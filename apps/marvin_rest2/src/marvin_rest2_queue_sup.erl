-module(marvin_rest2_queue_sup).
-behaviour(supervisor).

-include_lib("marvin_helper/include/marvin_specs_supervisor.hrl").

-export([start_link/1, init/1]).
-export([]).



%% Interface



-spec start_link(RateLimitGroup :: atom()) ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link(RateLimitGroup) ->
    supervisor:start_link(?MODULE, [RateLimitGroup]).



init([RateLimitGroup]) ->
    {ok, {
        #{
            strategy => rest_for_one,
            intensity => 5,
            period => 10
        },
        [
            #{
                id => {marvin_rest2_queue, RateLimitGroup},
                start => {marvin_rest2_queue, start_link, [RateLimitGroup]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [marvin_rest2_queue]
            },
            #{
                id => {marvin_rest2_queue_executor, RateLimitGroup},
                start => {marvin_rest2_queue_executor, start_link, [RateLimitGroup]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [marvin_rest2_queue_executor]
            }
        ]
    }}.



%% Internals
