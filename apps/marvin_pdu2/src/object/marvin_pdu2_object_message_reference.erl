-module(marvin_pdu2_object_message_reference).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    message_id = undefined :: message_id() | undefined,
    channel_id = undefined :: channel_id() | undefined,
    guild_id = undefined :: guild_id() | undefined
}).

-type message_id() :: marvin_pdu2:snowflake().
-type channel_id() :: marvin_pdu2:snowflake().
-type guild_id() :: marvin_pdu2:snowflake().
-type t() :: #?MODULE{}.

-export_type([message_id/0, channel_id/0, guild_id/0, t/0]).

cloak_validate(_, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value}.


export(#?MODULE{
    message_id = MessageId,
    channel_id = ChannelId,
    guild_id = GuildId
}) ->
    #{
        <<"message_id">> => marvin_pdu2:nullify(MessageId),
        <<"channel_id">> => marvin_pdu2:nullify(ChannelId),
        <<"guild_id">> => marvin_pdu2:nullify(GuildId)
    }.
