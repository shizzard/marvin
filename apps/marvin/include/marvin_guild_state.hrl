-record(state, {
    guild_id :: marvin_pdu2:snowflake(),
    guild_config :: marvin_guild_config:t(),
    owner_id :: marvin_pdu2:snowflake() | undefined,
    presence_state :: ets:tid(),
    role_state :: ets:tid(),
    emoji_state :: ets:tid(),
    channel_text_state :: ets:tid(),
    channel_voice_state :: ets:tid(),
    channel_category_state :: ets:tid(),
    member_state :: ets:tid(),
    voice_state :: ets:tid(),
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

-record(channel, {
    channel_id :: marvin_pdu2:snowflake(),
    channel :: marvin_pdu2_object_channel:t()
}).

-record(member, {
    member_id :: marvin_pdu2:snowflake(),
    member :: marvin_pdu2_object_member:t()
}).

-record(voice_state, {
    user_id :: marvin_pdu2:snowflake(),
    channel_id :: marvin_pdu2:snowflake()
}).
