-module(marvin_sup).
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
    {ok, {{one_for_one, 5, 10}, [
        {marvin_guild_sup, {
            marvin_guild_sup, start_link, []
        }, permanent, 5000, supervisor, [marvin_guild_sup]},
        {marvin_shard_sup, {
            marvin_shard_sup, start_link, []
        }, permanent, 5000, supervisor, [marvin_shard_sup]},
        {marvin_gateway_meta, {
            marvin_gateway_meta, start_link, []
        }, permanent, 5000, worker, [marvin_gateway_meta]}
    ]}}.
