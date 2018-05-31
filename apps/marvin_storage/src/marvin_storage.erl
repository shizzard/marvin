-module(marvin_storage).

-export([set_user_info/4, get_user_info/3]).



%% Interface



set_user_info(PluginId, GuildId, UserId, Data) ->
    poolboy:transaction(marvin_storage, fun(Worker) ->
        mc_worker_api:update(
            Worker, GuildId, #{
                <<"_id">> => UserId, <<"_plugin_id">> => PluginId
            }, #{
                <<"_data">> => Data, <<"_plugin_id">> => PluginId
            }, _Upsert = true, _Multi = false
        )
    end).



get_user_info(PluginId, GuildId, UserId) ->
    poolboy:transaction(marvin_storage, fun(Worker) ->
        mc_worker_api:find_one(
            Worker, GuildId, #{
                <<"_id">> => UserId, <<"_plugin_id">> => PluginId
            }
        )
    end).
