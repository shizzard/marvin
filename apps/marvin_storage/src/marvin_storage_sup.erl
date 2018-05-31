-module(marvin_storage_sup).
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
    {ok, DbPoolSize} = marvin_config:get_integer(marvin, [db, pool_size]),
    {ok, DbPoolOverflow} = marvin_config:get_integer(marvin, [db, pool_overflow]),
    {ok, DbName} = marvin_config:get(marvin, [db, name]),
    {ok, DbHost} = marvin_config:get(marvin, [db, host]),
    {ok, DbPort} = marvin_config:get_integer(marvin, [db, port]),
    PoolArgs = [
        {name, {local, marvin_storage}},
        {worker_module, mc_worker},
        {size, DbPoolSize},
        {max_overflow, DbPoolOverflow},
        {strategy, fifo}
    ],
    WorkerArgs = [
        {database, DbName},
        {host, DbHost},
        {port, DbPort},
        {next_req_fun, fun() -> poolboy:checkin(marvin_storage, self()) end}
    ],
    {ok, {{one_for_one, 2, 10}, [
        poolboy:child_spec(marvin_storage, PoolArgs, WorkerArgs)
    ]}}.
