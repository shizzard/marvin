-record(state, {
    config :: marvin_plugin_config:t(),
    guild_id :: marvin_pdu2:snowflake(),
    game_roles :: [marvin_pdu2:snowflake()]
}).
-type state() :: #state{}.
