-module(marvin_pdu2_object_guild_channel).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: id(),
    type :: type(),
    parent_id = undefined :: parent_id() | undefined,
    name :: name(),
    topic = undefined :: topic() | undefined,
    last_message_id = undefined :: last_message_id() | undefined,
    position :: position(),
    user_limit = undefined :: user_limit() | undefined,
    bitrate = undefined :: bitrate() | undefined,
    permission_overwrites :: permission_overwrites()
}).

-define(channel_type_guild_text, 0).
-define(channel_type_guild_voice, 2).
-define(channel_type_guild_category, 4).

-type id() :: marvin_pdu2:snowflake().
-type type() :: ?channel_type_guild_text | ?channel_type_guild_voice | ?channel_type_guild_category.
-type parent_id() :: marvin_pdu2:snowflake().
-type name() :: unicode:unicode_binary().
-type topic() :: unicode:unicode_binary().
-type last_message_id() :: marvin_pdu2:snowflake().
-type position() :: non_neg_integer().
-type user_limit() :: non_neg_integer().
-type bitrate() :: pos_integer().
-type permission_overwrites() :: [marvin_pdu2_object_permission_overwrite:t()].
-type t() :: #?MODULE{}.

-export_type([
    id/0, type/0, parent_id/0, name/0, topic/0, last_message_id/0, position/0,
    user_limit/0, bitrate/0, permission_overwrites/0, t/0
]).


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(type, Value)
when ?channel_type_guild_text == Value
orelse ?channel_type_guild_voice == Value
orelse ?channel_type_guild_category == Value ->
    {ok, Value};

cloak_validate(parent_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(topic, Value) when is_binary(Value) ->
    {ok, Value};

cloak_validate(last_message_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(position, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(user_limit, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(bitrate, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(permission_overwrites, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_permission_overwrite:new(Item) || Item <- Value]};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    id = Id,
    type = Type,
    parent_id = ParentId,
    name = Name,
    topic = Topic,
    last_message_id = LastMessageId,
    position = Position,
    user_limit = UserLimit,
    bitrate = Bitrate,
    permission_overwrites = PermissionOverwrites
}) ->
    #{
        <<"id">> => Id,
        <<"type">> => Type,
        <<"parent_id">> => ParentId,
        <<"name">> => Name,
        <<"topic">> => Topic,
        <<"last_message_id">> => LastMessageId,
        <<"position">> => Position,
        <<"user_limit">> => UserLimit,
        <<"bitrate">> => Bitrate,
        <<"permission_overwrites">> => [marvin_pdu2_object_permission_overwrite:export(Item) || Item <- PermissionOverwrites]
    }.
