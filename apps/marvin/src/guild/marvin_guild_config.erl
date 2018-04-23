-module(marvin_guild_config).
-compile({parse_transform, cloak_transform}).

-export([load/1, save/1]).

-record(?MODULE, {
    guild_id :: marvin_pdu2:snowflake(),
    role_admin = undefined :: marvin_pdu2:snowflake(),
    role_moderator = undefined :: marvin_pdu2:snowflake(),
    enabled_plugins = [] :: [atom()]
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



%% Interface



cloak_validate(guild_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(role_admin, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(role_moderator, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(enabled_plugins, Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.



-spec load(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(
        OkRet :: t()
    ).

load(GuildId) ->
    load_impl(GuildId).


-spec save(Config :: t()) ->
    marvin_helper_type:ok_return().

save(Config) ->
    save_impl(Config).



%% Internals



-spec get_config_filename(GuildId :: marvin_pdu2:snowflake()) ->
    Ret :: unicode:unicode_binary().

get_config_filename(GuildId) ->
    {ok, GuildConfigRoot} = marvin_config:get(marvin, [guild, config_root]),
    {ok, GuildConfigFilenameTemplate} = marvin_config:get(marvin, [guild, config_filename_template]),
    <<
        GuildConfigRoot/binary,
        (binary:replace(GuildConfigFilenameTemplate, <<"{{guild_id}}">>, GuildId))/binary
    >>.



-spec load_impl(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(
        OkRet :: t()
    ).

load_impl(GuildId) ->
    ConfigFilename = get_config_filename(GuildId),
    try
        case file:read_file(ConfigFilename) of
            {ok, Binary} -> {ok, new(jiffy:decode(Binary, [return_maps]))};
            {error, Reason} -> throw({read_failed, Reason})
        end
    catch CatchType:CatchReason ->
        marvin_log:error(
            "Guild '~s' config ('~s') read failed with reason '~p:~p', generating new config",
            [GuildId, ConfigFilename, CatchType, CatchReason]
        ),
        Config = new(#{guild_id => GuildId}),
        save(Config),
        {ok, Config}
    end.



-spec save_impl(Config :: t()) ->
    marvin_helper_type:ok_return().

save_impl(#?MODULE{guild_id = GuildId} = Config) ->
    ConfigFilename = get_config_filename(GuildId),
    try
        case file:write_file(ConfigFilename, jiffy:encode(export(Config))) of
            ok -> ok;
            {error, Reason} -> throw({write_failed, Reason})
        end
    catch CatchType:CatchReason ->
        marvin_log:error(
            "Guild '~s' config ('~s') write failed with reason '~p:~p'",
            [GuildId, ConfigFilename, CatchType, CatchReason]
        ),
        ok
    end.



export(#?MODULE{
    guild_id = GuildId,
    role_admin = RoleAdmin,
    role_moderator = RoleModerator,
    enabled_plugins = EnabledPlugins
}) ->
    clean(#{
        <<"guild_id">> => GuildId,
        <<"role_admin">> => RoleAdmin,
        <<"role_moderator">> => RoleModerator,
        <<"enabled_plugins">> => EnabledPlugins
    }).



-spec clean(Map :: map()) ->
    Ret :: map().

clean(Map) ->
    %% Copypasted from marvin_pdu2:export
    maps:from_list([{Key, Value} || {Key, Value} <- maps:to_list(Map), Value /= undefined]).
