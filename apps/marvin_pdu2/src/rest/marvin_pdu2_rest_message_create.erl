-module(marvin_pdu2_rest_message_create).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    content = undefined :: content() | undefined,
    message_reference = undefined :: message_reference() | undefined,
    allowed_mentions = undefined :: allowed_mentions() | undefined,
    nonce = undefined :: nonce() | undefined,
    tts = undefined :: tts() | undefined,
    embed = undefined :: embed() | undefined
}).

-type content() :: unicode:unicode_binary().
-type message_reference() :: marvin_pdu2_object_message_reference:t().
-type allowed_mentions() :: marvin_pdu2_object_allowed_mentions:t().
-type nonce() :: marvin_pdu2:snowflake().
-type tts() :: boolean().
-type embed() :: marvin_pdu2_object_embed:t().
-type t() :: #?MODULE{}.

-export_type([content/0, message_reference/0, allowed_mentions/0, nonce/0, tts/0, embed/0, t/0]).


cloak_validate(content, Value) when is_binary(Value) ->
    {ok, Value};

cloak_validate(message_reference, Value) ->
    {ok, marvin_pdu2_object_message_reference:new(Value)};

cloak_validate(allowed_mentions, Value) ->
    {ok, marvin_pdu2_object_allowed_mentions:new(Value)};

cloak_validate(nonce, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(tts, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(embed, Value) ->
    {ok, marvin_pdu2_object_embed:new(Value)};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    content = Content,
    message_reference = MessageReference,
    allowed_mentions = AllowedMentions,
    nonce = Nonce,
    tts = Tts,
    embed = Embed
}) ->
    #{
        <<"content">> => marvin_pdu2:nullify(Content),
        <<"message_reference">> => case MessageReference of
            undefined -> marvin_pdu2:nullify(MessageReference);
            _ -> marvin_pdu2_object_message_reference:export(MessageReference)
        end,
        <<"allowed_mentions">> => case AllowedMentions of
            undefined -> marvin_pdu2:nullify(AllowedMentions);
            _ -> marvin_pdu2_object_allowed_mentions:export(AllowedMentions)
        end,
        <<"nonce">> => marvin_pdu2:nullify(Nonce),
        <<"tts">> => marvin_pdu2:nullify(Tts),
        <<"embed">> => case Embed of
            undefined -> marvin_pdu2:nullify(Embed);
            _ -> marvin_pdu2_object_embed:export(Embed)
        end
    }.
