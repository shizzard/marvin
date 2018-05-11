-module(marvin_rest_sup).
-behaviour(supervisor).
-include_lib("marvin_helper/include/marvin_specs_supervisor.hrl").

-export([start_link/0, init/1]).



%% Interface



-spec start_link() ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{one_for_one, 2, 10}, [
        {marvin_rest_ratelimit_watchdog, {
            marvin_rest_ratelimit_watchdog, start_link, []
        }, permanent, 5000, worker, [marvin_rest_ratelimit_watchdog]}
    ]}}.
