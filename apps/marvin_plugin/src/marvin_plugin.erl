-module(marvin_plugin).

-type id() :: binary().
-export_type([id/0]).

-callback start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

-callback get_commands(L10n :: binary()) ->
    [marvin_plugin_command:t()].
