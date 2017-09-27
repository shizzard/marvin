-module(marvin_pdu_object_guild_unavailable).
-behavior(marvin_pdu_object).
-include("marvin_discord.hrl").
-include("marvin_pdu_object.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, data_map/0, export/1]).
-export([id/1, id/2]).



%% Types



-type id() :: marvin_pdu:snowflake().

-export_type([id/0]).

-record(object, {
    id :: id()
}).
-type object_internal() :: #object{}.
-type object() :: ?marvin_pdu_object_guild_unavailable(PDU :: object_internal()).



%% Interface



-spec new(Data :: #{} | id()) ->
    Ret :: object().

new(#{?discord_key_object_guild_unavailable_id := Id}) ->
    new(Id);

new(Id) ->
    ?marvin_pdu_object_guild_unavailable(#object{id = Id}).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_object_guild_unavailable_id, required, jiffy_vm:integer(fun validate_id/3))
    ]).



-spec export(PDU :: object()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_object_guild_unavailable(#object{
    id = Id
})) ->
    {ok, #{
        ?discord_key_object_guild_unavailable_id => Id,
        ?discord_key_object_guild_unavailable_unavailable => true
    }}.



-spec id(Object :: object()) ->
    Ret :: id().

id(?marvin_pdu_object_guild_unavailable(#object{id = Value})) ->
    Value.



-spec id(Value :: id(), Object :: object()) ->
    Ret :: object().

id(Value, ?marvin_pdu_object_guild_unavailable(#object{} = Object)) ->
    ?marvin_pdu_object_guild_unavailable(Object#object{id = Value}).



%% Internals



-spec ?validator_spec(validate_id).

validate_id(validate, _, _Id) ->
    {ok, valid};

validate_id(fix, _, Id) when is_binary(Id) ->
    {ok, binary_to_integer(Id)}.
