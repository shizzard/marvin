%% Definitions

%% OP codes

-define(discord_op_dispatch, 0).
-define(discord_op_heartbeat, 1).
-define(discord_op_identify, 2).
-define(discord_op_status_update, 3).
-define(discord_op_voice_state_update, 4).
-define(discord_op_voice_server_ping, 5).
-define(discord_op_resume, 6).
-define(discord_op_reconnect, 7).
-define(discord_op_request_guild_members, 8).
-define(discord_op_invalid_session, 9).
-define(discord_op_hello, 10).
-define(discord_op_heartbeat_ack, 11).
-define(discord_ops, [
    ?discord_op_dispatch, ?discord_op_heartbeat, ?discord_op_identify,
    ?discord_op_status_update, ?discord_op_voice_state_update,
    ?discord_op_voice_server_ping, ?discord_op_resume, ?discord_op_reconnect,
    ?discord_op_request_guild_members, ?discord_op_invalid_session,
    ?discord_op_hello, ?discord_op_heartbeat_ack
]).

%% Events

-define(discord_event_ready, <<"READY">>).
-define(discord_event_resumed, <<"RESUMED">>).
-define(discord_event_channel_create, <<"CHANNEL_CREATE">>).
-define(discord_event_channel_update, <<"CHANNEL_UPDATE">>).
-define(discord_event_channel_delete, <<"CHANNEL_DELETE">>).
-define(discord_event_guild_create, <<"GUILD_CREATE">>).
-define(discord_event_guild_update, <<"GUILD_UPDATE">>).
-define(discord_event_guild_delete, <<"GUILD_DELETE">>).
-define(discord_event_guild_ban_add, <<"GUILD_BAN_ADD">>).
-define(discord_event_guild_ban_remove, <<"GUILD_BAN_REMOVE">>).
-define(discord_event_guild_emojis_update, <<"GUILD_EMOJIS_UPDATE">>).
-define(discord_event_guild_integrations_update, <<"GUILD_INTEGRATIONS_UPDATE">>).
-define(discord_event_guild_member_add, <<"GUILD_MEMBER_ADD">>).
-define(discord_event_guild_member_remove, <<"GUILD_MEMBER_REMOVE">>).
-define(discord_event_guild_member_update, <<"GUILD_MEMBER_UPDATE">>).
-define(discord_event_guild_members_chunk, <<"GUILD_MEMBERS_CHUNK">>).
-define(discord_event_guild_role_create, <<"GUILD_ROLE_CREATE">>).
-define(discord_event_guild_role_update, <<"GUILD_ROLE_UPDATE">>).
-define(discord_event_guild_role_delete, <<"GUILD_ROLE_DELETE">>).
-define(discord_event_message_create, <<"MESSAGE_CREATE">>).
-define(discord_event_message_update, <<"MESSAGE_UPDATE">>).
-define(discord_event_message_delete, <<"MESSAGE_DELETE">>).
-define(discord_event_message_delete_bulk, <<"MESSAGE_DELETE_BULK">>).
-define(discord_event_message_reaction_add, <<"MESSAGE_REACTION_ADD">>).
-define(discord_event_message_reaction_remove, <<"MESSAGE_REACTION_REMOVE">>).
-define(discord_event_message_reaction_remove_all, <<"MESSAGE_REACTION_REMOVE_ALL">>).
-define(discord_event_presence_update, <<"PRESENCE_UPDATE">>).
-define(discord_event_typing_start, <<"TYPING_START">>).
-define(discord_event_user_update, <<"USER_UPDATE">>).
-define(discord_event_voice_state_update, <<"VOICE_STATE_UPDATE">>).
-define(discord_event_voice_server_update, <<"VOICE_SERVER_UPDATE">>).

-define(discord_events, [
    ?discord_event_ready,
    ?discord_event_resumed,
    ?discord_event_channel_create,
    ?discord_event_channel_update,
    ?discord_event_channel_delete,
    ?discord_event_guild_create,
    ?discord_event_guild_update,
    ?discord_event_guild_delete,
    ?discord_event_guild_ban_add,
    ?discord_event_guild_ban_remove,
    ?discord_event_guild_emojis_update,
    ?discord_event_guild_integrations_update,
    ?discord_event_guild_member_add,
    ?discord_event_guild_member_remove,
    ?discord_event_guild_member_update,
    ?discord_event_guild_members_chunk,
    ?discord_event_guild_role_create,
    ?discord_event_guild_role_update,
    ?discord_event_guild_role_delete,
    ?discord_event_message_create,
    ?discord_event_message_update,
    ?discord_event_message_delete,
    ?discord_event_message_delete_bulk,
    ?discord_event_message_reaction_add,
    ?discord_event_message_reaction_remove,
    ?discord_event_message_reaction_remove_all,
    ?discord_event_presence_update,
    ?discord_event_typing_start,
    ?discord_event_user_update,
    ?discord_event_voice_state_update,
    ?discord_event_voice_server_update
]).

%% Generic PDU keys

-define(discord_key_op, <<"op">>).
-define(discord_key_data, <<"d">>).
-define(discord_key_seq, <<"s">>).
-define(discord_key_event, <<"t">>).

%% '2 Identify' PDU data keys

-define(discord_key_identify_token, <<"token">>).
-define(discord_key_identify_properties_os, <<"$os">>).
-define(discord_key_identify_properties_browser, <<"$browser">>).
-define(discord_key_identify_properties_device,<<"$device">>).
-define(discord_key_identify_properties_referrer,<<"$referrer">>).
-define(discord_key_identify_properties_referring_domain,<<"$referring_domain">>).
-define(discord_key_identify_properties, <<"properties">>).
-define(discord_key_identify_compress,<<"compress">>).
-define(discord_key_identify_large_threshold,<<"large_threshold">>).
-define(discord_key_identify_shard,<<"shard">>).

%% '11 Hello' PDU data keys

-define(discord_key_hello_heartbeat_interval, <<"heartbeat_interval">>).
-define(discord_key_hello__trace, <<"_trace">>).

%% ...

%% Generic object keys

%% User

-define(discord_key_object_user_id, <<"id">>).
-define(discord_key_object_user_username, <<"username">>).
-define(discord_key_object_user_discriminator, <<"discriminator">>).
-define(discord_key_object_user_avatar, <<"avatar">>).
-define(discord_key_object_user_bot, <<"bot">>).
-define(discord_key_object_user_mfa_enabled, <<"mfa_enabled">>).
-define(discord_key_object_user_verified, <<"verified">>).
-define(discord_key_object_user_email, <<"email">>).

%% DM channel

-define(discord_key_object_channel_dm_id, <<"id">>).
-define(discord_key_object_channel_dm_type, <<"type">>).
-define(discord_key_object_channel_dm_last_message_id, <<"last_message_id">>).
-define(discord_key_object_channel_dm_recipients, <<"recipients">>).

%% Guild unavailable

-define(discord_key_object_guild_unavailable_id, <<"id">>).
-define(discord_key_object_guild_unavailable_unavailable, <<"unavailable">>).
