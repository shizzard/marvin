-module(marvin_pdu_invalid_session).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, data_map/0, export/1]).
-export([resumable/1, resumable/2]).



%% Types



-type resumable() :: non_neg_integer().
-export_type([resumable/0]).

-record(pdu, {
    resumable :: resumable()
}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_invalid_session(PDU :: pdu_internal()).



%% Interface



-spec new(Resumable :: resumable()) ->
    Ret :: pdu().

new(Resumable) ->
    ?marvin_pdu_invalid_session(#pdu{
        resumable = Resumable
    }).



-spec data_map() ->
    jiffy_vm:jv_type_integer().

data_map() ->
    jiffy_vm:boolean().



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: resumable()).

export(?marvin_pdu_invalid_session(#pdu{
    resumable = Resumable
})) ->
    {ok, Resumable}.



-spec resumable(PDU :: pdu()) ->
    Ret :: resumable().

resumable(?marvin_pdu_invalid_session(#pdu{resumable = Value})) ->
    Value.



-spec resumable(
    Resumable :: resumable(),
    PDU :: pdu()
) ->
    Ret :: pdu().

resumable(Value, ?marvin_pdu_invalid_session(#pdu{} = PDU)) ->
    ?marvin_pdu_invalid_session(PDU#pdu{resumable = Value}).



%% Internals
