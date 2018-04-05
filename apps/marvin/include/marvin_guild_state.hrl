-record(state, {
    guild_id :: marvin_pdu2:snowflake(),
    owner_id :: marvin_pdu2:snowflake(),
    presence_state :: ets:tid(),
    role_state :: ets:tid(),
    emoji_state :: ets:tid(),
    members_online = 0 :: non_neg_integer(),
    members_idle = 0 :: non_neg_integer(),
    members_dnd = 0 :: non_neg_integer()
}).
-type state() :: #state{}.

-record(presence, {
    user_id :: marvin_pdu2:snowflake(),
    status :: marvin_pdu2_object_presence:status()
}).

-record(role, {
    role_id :: marvin_pdu2:snowflake(),
    role :: marvin_pdu2_object_role:t()
}).

-record(emoji, {
    emoji_id :: marvin_pdu2:snowflake(),
    emoji :: marvin_pdu2_object_emoji:t()
}).
