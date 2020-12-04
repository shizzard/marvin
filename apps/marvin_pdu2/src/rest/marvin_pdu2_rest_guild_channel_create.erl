-module(marvin_pdu2_rest_guild_channel_create).
-compile({parse_transform, cloak_transform}).

-export([
    export/1, channel_type_guild_text/0, channel_type_guild_voice/0, channel_type_guild_category/0
]).

-record(?MODULE, {
    name :: name(),
    type :: type(),
    topic = undefined :: topic(),
    bitrate = undefined :: bitrate(),
    user_limit = undefined :: user_limit(),
    permission_overwrites = [] :: permission_overwrites(),
    parent_id = undefined :: parent_id(),
    nsfw = false :: nsfw()
}).

-define(channel_type_guild_text, 0).
-define(channel_type_guild_voice, 2).
-define(channel_type_guild_category, 4).

-type name() :: unicode:unicode_binary().
-type type() :: ?channel_type_guild_text | ?channel_type_guild_voice | ?channel_type_guild_category.
-type topic() :: unicode:unicode_binary().
-type bitrate() :: 16000..384000.
-type user_limit() :: pos_integer().
-type permission_overwrites() :: [marvin_pdu2_object_permission_overwrite:t()].
-type parent_id() :: marvin_pdu2:snowflake().
-type nsfw() :: boolean().
-type t() :: #?MODULE{}.

-export_type([
    name/0, type/0, topic/0, bitrate/0, user_limit/0, permission_overwrites/0,
    parent_id/0, nsfw/0, t/0
]).


channel_type_guild_text() -> ?channel_type_guild_text.
channel_type_guild_voice() -> ?channel_type_guild_voice.
channel_type_guild_category() -> ?channel_type_guild_category.


cloak_validate(name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(type, Value)
when ?channel_type_guild_text == Value
orelse ?channel_type_guild_voice == Value
orelse ?channel_type_guild_category == Value ->
    {ok, Value};

cloak_validate(topic, null) ->
    {ok, undefined};

cloak_validate(topic, Value) when is_binary(Value) ->
    {ok, Value};

cloak_validate(bitrate, Value) when is_integer(Value) andalso Value >= 16000 andalso Value =< 384000 ->
    {ok, Value};

cloak_validate(user_limit, undefined) ->
    {ok, undefined};

cloak_validate(user_limit, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(permission_overwrites, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_permission_overwrite:new(Item) || Item <- Value]};

cloak_validate(parent_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(nsfw, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    name = Name,
    type = Type,
    topic = Topic,
    bitrate = Bitrate,
    user_limit = UserLimit,
    permission_overwrites = PermissionOverwrites,
    parent_id = ParentId,
    nsfw = Nsfw
}) ->
    #{
        <<"name">> => marvin_pdu2:nullify(Name),
        <<"topic">> => marvin_pdu2:nullify(Topic),
        <<"type">> => Type,
        <<"bitrate">> => marvin_pdu2:nullify(Bitrate),
        <<"user_limit">> => marvin_pdu2:nullify(UserLimit),
        <<"permission_overwrites">> => [marvin_pdu2_object_permission_overwrite:export(Item) || Item <- PermissionOverwrites],
        <<"parent_id">> => marvin_pdu2:nullify(ParentId),
        <<"nsfw">> => Nsfw
    }.
