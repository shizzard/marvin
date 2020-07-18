-module(marvin_pdu2_dispatch_channel_create).
-compile({parse_transform, cloak_transform}).

-export([
    export/1, format/1, channel_type_guild_text/0, channel_type_dm/0,
    channel_type_guild_voice/0, channel_type_group_dm/0, channel_type_guild_category/0
]).

-record(?MODULE, {
    id = undefined :: id(),
    guild_id = undefined :: guild_id() | undefined,
    type = undefined :: type(),
    parent_id = undefined :: parent_id() | undefined,
    name = undefined :: name() | undefined,
    topic = undefined :: topic() | undefined,
    nsfw = false :: nsfw(),
    last_message_id = undefined :: last_message_id() | undefined,
    position = undefined :: position() | undefined,
    user_limit = undefined :: user_limit() | undefined,
    recipients = [] :: recipients(),
    icon = undefined :: icon() | undefined,
    owner_id = undefined :: owner_id() | undefined,
    application_id = undefined :: application_id() | undefined,
    bitrate = undefined :: bitrate() | undefined,
    permission_overwrites = [] :: permission_overwrites() | undefined,
    last_pin_timestamp = undefined :: last_pin_timestamp() | undefined
}).

-define(channel_type_guild_text, 0).
-define(channel_type_dm, 1).
-define(channel_type_guild_voice, 2).
-define(channel_type_group_dm, 3).
-define(channel_type_guild_category, 4).

-type id() :: marvin_pdu2:snowflake().
-type guild_id() :: marvin_pdu2:snowflake().
-type type() :: ?channel_type_guild_text..?channel_type_guild_category.
-type parent_id() :: marvin_pdu2:snowflake().
-type name() :: unicode:unicode_binary().
-type topic() :: unicode:unicode_binary().
-type nsfw() :: boolean().
-type last_message_id() :: marvin_pdu2:snowflake().
-type position() :: non_neg_integer().
-type user_limit() :: non_neg_integer().
-type recipients() :: [marvin_pdu2_object_user:t()].
-type owner_id() :: marvin_pdu2:snowflake().
-type application_id() :: marvin_pdu2:snowflake().
-type icon() :: unicode:unicode_binary().
-type bitrate() :: pos_integer().
-type permission_overwrites() :: [marvin_pdu2_object_permission_overwrite:t()].
-type last_pin_timestamp() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([
    id/0, guild_id/0, type/0, parent_id/0, name/0, topic/0, nsfw/0, last_message_id/0, position/0,
    user_limit/0, recipients/0, owner_id/0, icon/0, bitrate/0, permission_overwrites/0, last_pin_timestamp/0, t/0
]).


format(#?MODULE{id = Id}) ->
    <<"<#", Id/binary, ">">>.


channel_type_guild_text() -> ?channel_type_guild_text.
channel_type_dm() -> ?channel_type_dm.
channel_type_guild_voice() -> ?channel_type_guild_voice.
channel_type_group_dm() -> ?channel_type_group_dm.
channel_type_guild_category() -> ?channel_type_guild_category.

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    id = Id,
    guild_id = GuildId,
    type = Type,
    parent_id = ParentId,
    name = Name,
    topic = Topic,
    nsfw = Nsfw,
    last_message_id = LastMessageId,
    position = Position,
    user_limit = UserLimit,
    recipients = Recipients,
    owner_id = OwnerId,
    application_id = ApplicationId,
    icon = Icon,
    bitrate = Bitrate,
    permission_overwrites = PermissionOverwrites,
    last_pin_timestamp = LastPinTimestamp
}) ->
    marvin_pdu2:export(#{
        <<"id">> => Id,
        <<"guild_id">> => GuildId,
        <<"type">> => Type,
        <<"parent_id">> => marvin_pdu2:nullify(ParentId),
        <<"name">> => Name,
        <<"topic">> => marvin_pdu2:nullify(Topic),
        <<"nsfw">> => Nsfw,
        <<"last_message_id">> => marvin_pdu2:nullify(LastMessageId),
        <<"position">> => Position,
        <<"user_limit">> => UserLimit,
        <<"recipients">> => [marvin_pdu2_object_user:export(Item) || Item <- Recipients],
        <<"owner_id">> => OwnerId,
        <<"application_id">> => ApplicationId,
        <<"icon">> => marvin_pdu2:nullify(Icon),
        <<"bitrate">> => Bitrate,
        <<"permission_overwrites">> => [marvin_pdu2_object_permission_overwrite:export(Item) || Item <- PermissionOverwrites],
        <<"last_pin_timestamp">> => LastPinTimestamp
    }).
