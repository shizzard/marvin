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

-define(discord_events, []).

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
