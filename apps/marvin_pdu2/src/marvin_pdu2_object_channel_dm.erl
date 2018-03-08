-module(marvin_pdu2_object_channel_dm).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: marvin_pdu2:snowflake(),
    type :: marvin_pdu2:channel_type_dm(),
    last_message_id :: marvin_pdu2:snowflake(),
    recipients :: [marvin_pdu2_object_user:t()]
}).

-type t() :: #?MODULE{}.
-export_type([t/0]).



cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(type, 1) ->
    {ok, 1};

cloak_validate(last_message_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(recipients, Value) ->
    {ok, [marvin_pdu2_object_user:new(User) || User <- Value]};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    id = Id,
    type = Type,
    last_message_id = LastMessageId,
    recipients = Recipients
}) ->
    #{
        <<"id">> => Id,
        <<"type">> => Type,
        <<"last_message_id">> => LastMessageId,
        <<"recipients">> => [marvin_pdu2_object_user:export(User) || User <- Recipients]
    }.
