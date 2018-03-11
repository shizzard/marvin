-module(marvin_pdu2_object_channel_dm).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: id(),
    type :: type(),
    last_message_id :: last_message_id(),
    recipients :: recipients()
}).

-type id() :: marvin_pdu2:snowflake().
-type type() :: 1.
-type last_message_id() :: marvin_pdu2:snowflake().
-type recipients() :: [marvin_pdu2_object_user:t()].
-type t() :: #?MODULE{}.

-export_type([id/0, type/0, last_message_id/0, recipients/0, t/0]).



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
