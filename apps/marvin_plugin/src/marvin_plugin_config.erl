-module(marvin_plugin_config).
-compile({parse_transform, cloak_transform}).

-include_lib("marvin_log/include/marvin_log.hrl").

-export([load/2, save/1, export/1]).

-record(?MODULE, {
    guild_id :: marvin_pdu2:snowflake(),
    plugin_id :: binary(),
    data :: term()
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



%% Interface



export(#?MODULE{
    guild_id = GuildId,
    plugin_id = PluginId,
    data = Data
}) ->
    #{
        <<"guild_id">> => GuildId,
        <<"plugin_id">> => PluginId,
        <<"data">> => Data
    }.



-spec load(PluginId :: marvin_plugin:id(), GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(
        OkRet :: t()
    ).

load(PluginId, GuildId) ->
    ConfigFilename = get_config_filename(PluginId, GuildId),
    try
        case file:read_file(ConfigFilename) of
            {ok, Binary} -> {ok, new(jiffy:decode(Binary, [return_maps]))};
            {error, Reason} -> throw({read_failed, Reason})
        end
    catch CatchType:CatchReason ->
        ?l_error(#{
            text => "Plugin config read failed, generating new one",
            what => load,
            details => #{
                type => CatchType, reason => CatchReason,
                plugin_id => PluginId, guild_id => GuildId,
                filename => ConfigFilename
            }
        }),
        Config = new(#{guild_id => GuildId, plugin_id => PluginId, data => #{}}),
        save(Config),
        {ok, Config}
    end.


-spec save(Config :: t()) ->
    marvin_helper_type:ok_return().

save(Config) ->
    ConfigFilename = get_config_filename(plugin_id(Config), guild_id(Config)),
    try
        case file:write_file(ConfigFilename, jiffy:encode(export(Config))) of
            ok -> ok;
            {error, Reason} -> throw({write_failed, Reason})
        end
    catch CatchType:CatchReason ->
        ?l_error(#{
            text => "Plugin config write failed",
            what => load,
            details => #{
                type => CatchType, reason => CatchReason,
                plugin_id => plugin_id(Config), guild_id => guild_id(Config),
                filename => ConfigFilename
            }
        }),
        ok
    end.



%% Internals



-spec get_config_filename(PluginId :: marvin_plugin:id(), GuildId :: marvin_pdu2:snowflake()) ->
    Ret :: unicode:unicode_binary().

get_config_filename(PluginId, GuildId) ->
    {ok, PluginConfigRoot} = marvin_config:get(marvin, [plugin, config_root]),
    {ok, PluginConfigFilenameTemplate} = marvin_config:get(marvin, [plugin, config_filename_template]),
    <<
        PluginConfigRoot/binary,
        (binary:replace(
            binary:replace(PluginConfigFilenameTemplate, <<"{{plugin_id}}">>, PluginId),
            <<"{{guild_id}}">>, GuildId)
        )/binary
    >>.
