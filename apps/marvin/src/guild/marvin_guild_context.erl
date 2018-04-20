-module(marvin_guild_context).
-compile({parse_transform, cloak_transform}).

-record(?MODULE, {
    guild_id :: marvin_pdu2:snowflake(),
    owner_id :: marvin_pdu2:snowflake(),
    presence_state :: ets:tid(),
    role_state :: ets:tid(),
    emoji_state :: ets:tid(),
    channel_text_state :: ets:tid(),
    channel_voice_state :: ets:tid(),
    channel_category_state :: ets:tid(),
    member_state :: ets:tid(),
    voice_state :: ets:tid()
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).
