-module(marvin_plugin_config).
-compile({parse_transform, cloak_transform}).

-export([load/2, save/1, export/1]).

-record(?MODULE, {
    guild_id :: marvin_pdu2:snowflake(),
    plugin_id :: binary()
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



%% Interface



export(#?MODULE{
    guild_id = GuildId,
    plugin_id = PluginId
}) ->
    #{
        <<"guild_id">> => GuildId,
        <<"plugin_id">> => PluginId
    }.



-spec load(PluginId :: marvin_plugin:id(), GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(
        OkRet :: t()
    ).

load(PluginId, GuildId) ->
    load_impl(PluginId, GuildId).


-spec save(Config :: t()) ->
    marvin_helper_type:ok_return().

save(Config) ->
    save_impl(Config).



%% Internals



-spec get_config_filename(PluginId :: marvin_plugin:id(), GuildId :: marvin_pdu2:snowflake()) ->
    Ret :: unicode:unicode_binary().

get_config_filename(PluginId, GuildId) ->
    {ok, PluginConfigRoot} = marvin_config:get(marvin, [guild, config_root]),
    {ok, PluginConfigFilenameTemplate} = marvin_config:get(marvin, [plugin, config_filename_template]),
    <<
        PluginConfigRoot/binary,
        (binary:replace(
            binary:replace(PluginConfigFilenameTemplate, <<"{{plugin_id}}">>, PluginId),
            <<"{{guild_id}}">>, GuildId)
        )/binary
    >>.



-spec load_impl(PluginId :: marvin_plugin:id(), GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(
        OkRet :: t()
    ).

load_impl(PluginId, GuildId) ->
    ConfigFilename = get_config_filename(PluginId, GuildId),
    try
        case file:read_file(ConfigFilename) of
            {ok, Binary} -> {ok, new(jiffy:decode(Binary, [return_maps]))};
            {error, Reason} -> throw({read_failed, Reason})
        end
    catch CatchType:CatchReason ->
        marvin_log:error(
            "Plugin '~s' config for guild '~s' ('~s') read failed with reason '~p:~p', generating new config",
            [PluginId, GuildId, ConfigFilename, CatchType, CatchReason]
        ),
        Config = new(#{guild_id => GuildId, plugin_id => PluginId}),
        save(Config),
        {ok, Config}
    end.



-spec save_impl(Config :: t()) ->
    marvin_helper_type:ok_return().

save_impl(#?MODULE{guild_id = GuildId, plugin_id = PluginId} = Config) ->
    ConfigFilename = get_config_filename(PluginId, GuildId),
    try
        case file:write_file(ConfigFilename, jiffy:encode(export(Config))) of
            ok -> ok;
            {error, Reason} -> throw({write_failed, Reason})
        end
    catch CatchType:CatchReason ->
        marvin_log:error(
            "Plugin '~s' config for guild '~s' ('~s') write failed with reason '~p:~p'",
            [PluginId, GuildId, ConfigFilename, CatchType, CatchReason]
        ),
        ok
    end.
