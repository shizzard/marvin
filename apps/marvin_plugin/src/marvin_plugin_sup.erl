-module(marvin_plugin_sup).
-behaviour(supervisor).
-include_lib("marvin_helper/include/marvin_specs_supervisor.hrl").

-export([start_link/0, init/1]).
-export([start_plugin/2, stop_plugin/2]).

-define(plugin(PluginId, GuildId), {plugin, GuildId, PluginId}).



%% Interface



-spec start_plugin(PluginId :: atom(), GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid() | undefined) |
    marvin_helper_type:ok_return(OkRet1 :: pid() | undefined, OkRet2 :: term()) |
    marvin_helper_type:error_return(ErrorRet :: already_present | {already_started, Child :: pid() | undefined} | term()).

start_plugin(PluginId, GuildId) ->
    supervisor:start_child(?MODULE, {
        ?plugin(PluginId, GuildId), {
            PluginId, start_link, [GuildId]
        }, permanent, 15000, worker, [PluginId]
    }).



-spec stop_plugin(PluginId :: marvin_plugin:id(), GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return().

stop_plugin(PluginId, GuildId) ->
    supervisor:terminate_child(?MODULE, ?plugin(PluginId, GuildId)),
    supervisor:delete_child(?MODULE, ?plugin(PluginId, GuildId)).



-spec start_link() ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.



%% Internals
