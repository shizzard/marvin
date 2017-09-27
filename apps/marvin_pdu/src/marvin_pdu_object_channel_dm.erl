-module(marvin_pdu_object_channel_dm).
-behavior(marvin_pdu_object).
-include("marvin_discord.hrl").
-include("marvin_pdu_object.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, new/4, data_map/0, export/1]).
-export([
    id/1, type/1, last_message_id/1, recipients/1,
    id/2, type/2, last_message_id/2, recipients/2
]).



%% Types



-type id() :: marvin_pdu:snowflake().
-type type() :: 1.
-type last_message_id() :: marvin_pdu:snowflake().
-type recipients() :: [marvin_pdu_object_user:object(), ...].

-export_type([
    id/0, type/0, last_message_id/0, recipients/0
]).

-record(object, {
    id :: id(),
    type :: type(),
    last_message_id :: last_message_id(),
    recipients :: recipients()
}).
-type object_internal() :: #object{}.
-type object() :: ?marvin_pdu_object_channel_dm(PDU :: object_internal()).



%% Interface



-spec new(Data :: #{}) ->
    Ret :: object().

new(#{
    ?discord_key_object_channel_dm_id := Id,
    ?discord_key_object_channel_dm_type := Type,
    ?discord_key_object_channel_dm_last_message_id := LastMessageId,
    ?discord_key_object_channel_dm_recipients := Recipients
}) ->
    RecipientObjects = [marvin_pdu_object_user:new(Recipient) || Recipient <- Recipients],
    new(Id, Type, LastMessageId, RecipientObjects).



-spec new(
    Id :: id(),
    Type :: type(),
    LastMessageId :: last_message_id(),
    Recipients :: recipients()
) ->
    Ret :: object().

new(Id, Type, LastMessageId, Recipients) ->
    ?marvin_pdu_object_channel_dm(#object{
        id = Id,
        type = Type,
        last_message_id = LastMessageId,
        recipients = Recipients
    }).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_object_channel_dm_id, required, jiffy_vm:integer(fun validate_id/3)),
        jiffy_vm:hashfield(?discord_key_object_channel_dm_type, required, jiffy_vm:integer(fun validate_type/3)),
        jiffy_vm:hashfield(?discord_key_object_channel_dm_last_message_id, required, jiffy_vm:string(fun validate_id/3)),
        jiffy_vm:hashfield(?discord_key_object_channel_dm_recipients, required, jiffy_vm:list(
            [marvin_pdu_object_user:data_map()]
        ))
    ]).



-spec export(PDU :: object()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_object_channel_dm(#object{
    id = Id,
    type = Type,
    last_message_id = LastMessageId,
    recipients = RecipientObjects
})) ->
    Recipients = lists:map(fun(RecipientObject) ->
        {ok, Recipient} = marvin_pdu_object_user:export(RecipientObject),
        Recipient
    end, RecipientObjects),
    {ok, #{
        ?discord_key_object_channel_dm_id => Id,
        ?discord_key_object_channel_dm_type => Type,
        ?discord_key_object_channel_dm_last_message_id => LastMessageId,
        ?discord_key_object_channel_dm_recipients => Recipients
    }}.



-spec id(Object :: object()) ->
    Ret :: id().

id(?marvin_pdu_object_channel_dm(#object{id = Value})) ->
    Value.



-spec id(Value :: id(), Object :: object()) ->
    Ret :: object().

id(Value, ?marvin_pdu_object_channel_dm(#object{} = Object)) ->
    ?marvin_pdu_object_channel_dm(Object#object{id = Value}).



-spec type(Object :: object()) ->
    Ret :: type().

type(?marvin_pdu_object_channel_dm(#object{type = Value})) ->
    Value.



-spec type(Value :: type(), Object :: object()) ->
    Ret :: object().

type(Value, ?marvin_pdu_object_channel_dm(#object{} = Object)) ->
    ?marvin_pdu_object_channel_dm(Object#object{type = Value}).



-spec last_message_id(Object :: object()) ->
    Ret :: last_message_id().

last_message_id(?marvin_pdu_object_channel_dm(#object{last_message_id = Value})) ->
    Value.



-spec last_message_id(Value :: last_message_id(), Object :: object()) ->
    Ret :: object().

last_message_id(Value, ?marvin_pdu_object_channel_dm(#object{} = Object)) ->
    ?marvin_pdu_object_channel_dm(Object#object{last_message_id = Value}).



-spec recipients(Object :: object()) ->
    Ret :: recipients().

recipients(?marvin_pdu_object_channel_dm(#object{recipients = Value})) ->
    Value.



-spec recipients(Value :: recipients(), Object :: object()) ->
    Ret :: object().

recipients(Value, ?marvin_pdu_object_channel_dm(#object{} = Object)) ->
    ?marvin_pdu_object_channel_dm(Object#object{recipients = Value}).



%% Internals



-spec ?validator_spec(validate_id).

validate_id(validate, _, _Id) ->
    {ok, valid};

validate_id(fix, _, Id) when is_binary(Id) ->
    {ok, binary_to_integer(Id)};

validate_id(fix, _, Id) when is_integer(Id) ->
    {ok, integer_to_binary(Id)}.



-spec ?validator_spec(validate_type).

validate_type(validate, _, 1) ->
    {ok, valid};

validate_type(fix, _, _) ->
    {error, <<"Invalid type for DM channel">>}.
