-module(marvin_plugin).

-callback start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

