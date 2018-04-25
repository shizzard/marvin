-module(marvin_pdu2_object_permissions).

-export([empty/0, create/1, add/2, remove/2, have/2]).
-export([
    create_instant_invite/0, kick_members/0, ban_members/0, administrator/0,
    manage_channels/0, manage_guild/0, add_reactions/0, view_audit_log/0,
    view_channel/0, send_messages/0, send_tts_messages/0, manage_messages/0,
    embed_links/0, attach_files/0, read_message_history/0, mention_everyone/0,
    use_external_emojis/0, connect/0, speak/0, mute_members/0, deafen_members/0,
    move_members/0, use_vad/0, change_nickname/0, manage_nicknames/0,
    manage_roles/0, manage_webhooks/0, manage_emojis/0
]).

-type permission() :: pos_integer().
-type permission_compilation() :: pos_integer().

-define(CREATE_INSTANT_INVITE, 16#00000001).
-define(KICK_MEMBERS, 16#00000002).
-define(BAN_MEMBERS, 16#00000004).
-define(ADMINISTRATOR, 16#00000008).
-define(MANAGE_CHANNELS, 16#00000010).
-define(MANAGE_GUILD, 16#00000020).
-define(ADD_REACTIONS, 16#00000040).
-define(VIEW_AUDIT_LOG, 16#00000080).
-define(VIEW_CHANNEL, 16#00000400).
-define(SEND_MESSAGES, 16#00000800).
-define(SEND_TTS_MESSAGES, 16#00001000).
-define(MANAGE_MESSAGES, 16#00002000).
-define(EMBED_LINKS, 16#00004000).
-define(ATTACH_FILES, 16#00008000).
-define(READ_MESSAGE_HISTORY, 16#00010000).
-define(MENTION_EVERYONE, 16#00020000).
-define(USE_EXTERNAL_EMOJIS, 16#00040000).
-define(CONNECT, 16#00100000).
-define(SPEAK, 16#00200000).
-define(MUTE_MEMBERS, 16#00400000).
-define(DEAFEN_MEMBERS, 16#00800000).
-define(MOVE_MEMBERS, 16#01000000).
-define(USE_VAD, 16#02000000).
-define(CHANGE_NICKNAME, 16#04000000).
-define(MANAGE_NICKNAMES, 16#08000000).
-define(MANAGE_ROLES, 16#10000000).
-define(MANAGE_WEBHOOKS, 16#20000000).
-define(MANAGE_EMOJIS, 16#40000000).



%% Interface



-spec empty() ->
    Ret :: 16#0.

empty() ->
    16#0.



-spec create(List :: [permission(), ...]) ->
    Ret :: permission().

create([Permission | PermissionList]) ->
    add(Permission, PermissionList).



-spec add(PermissionCompilation :: permission_compilation(), List :: permission() | [permission(), ...]) ->
    Ret :: permission().

add(PermissionCompilation, Permission) when is_integer(Permission) ->
    add(PermissionCompilation, [Permission]);

add(PermissionCompilation, []) ->
    PermissionCompilation;

add(PermissionCompilation, [Permission | PermissionList]) ->
    add(PermissionCompilation bor Permission, PermissionList).



-spec remove(Permission :: permission_compilation(), List :: [permission(), ...]) ->
    Ret :: permission().

remove(PermissionCompilation, Permission) when is_integer(Permission) ->
    remove(PermissionCompilation, [Permission]);

remove(PermissionCompilation, []) ->
    PermissionCompilation;

remove(PermissionCompilation, [Permission | PermissionList]) ->
    remove(PermissionCompilation band (bnot Permission), PermissionList).



-spec have(PermissionCompilation :: permission_compilation(), Permission :: permission()) ->
    Ret :: boolean().

have(PermissionCompilation, Permission) ->
    0 /= PermissionCompilation band Permission.





create_instant_invite() -> ?CREATE_INSTANT_INVITE.
kick_members() -> ?KICK_MEMBERS.
ban_members() -> ?BAN_MEMBERS.
administrator() -> ?ADMINISTRATOR.
manage_channels() -> ?MANAGE_CHANNELS.
manage_guild() -> ?MANAGE_GUILD.
add_reactions() -> ?ADD_REACTIONS.
view_audit_log() -> ?VIEW_AUDIT_LOG.
view_channel() -> ?VIEW_CHANNEL.
send_messages() -> ?SEND_MESSAGES.
send_tts_messages() -> ?SEND_TTS_MESSAGES.
manage_messages() -> ?MANAGE_MESSAGES.
embed_links() -> ?EMBED_LINKS.
attach_files() -> ?ATTACH_FILES.
read_message_history() -> ?READ_MESSAGE_HISTORY.
mention_everyone() -> ?MENTION_EVERYONE.
use_external_emojis() -> ?USE_EXTERNAL_EMOJIS.
connect() -> ?CONNECT.
speak() -> ?SPEAK.
mute_members() -> ?MUTE_MEMBERS.
deafen_members() -> ?DEAFEN_MEMBERS.
move_members() -> ?MOVE_MEMBERS.
use_vad() -> ?USE_VAD.
change_nickname() -> ?CHANGE_NICKNAME.
manage_nicknames() -> ?MANAGE_NICKNAMES.
manage_roles() -> ?MANAGE_ROLES.
manage_webhooks() -> ?MANAGE_WEBHOOKS.
manage_emojis() -> ?MANAGE_EMOJIS.



