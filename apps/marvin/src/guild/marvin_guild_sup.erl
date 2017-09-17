-module(marvin_guild_sup).
-behaviour(supervisor).
-include_lib("marvin_helper/include/marvin_specs_supervisor.hrl").

-export([start_link/0, init/1]).
-export([start_guild/1, stop_guild/1]).



%% Interface



-spec start_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid() | undefined) |
    marvin_helper_type:ok_return(OkRet1 :: pid() | undefined, OkRet2 :: term()) |
    marvin_helper_type:error_return(ErrorRet :: already_present | {already_started, Child :: pid() | undefined} | term()).

start_guild(GuildId) ->
    supervisor:start_child(?MODULE, [GuildId]).



-spec stop_guild(Pid :: pid()) ->
    marvin_helper_type:ok_return().

stop_guild(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).



-spec start_link() ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [
        {marvin_guild, {
            marvin_guild, start_link, []
        }, temporary, 15000, worker, [marvin_guild]}
    ]}}.



%% Internals
