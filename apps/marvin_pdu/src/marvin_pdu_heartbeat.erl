-module(marvin_pdu_heartbeat).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, data_map/0, export/1]).
-export([last_seq/1, last_seq/2]).



%% Types



-type last_seq() :: non_neg_integer().
-export_type([last_seq/0]).

-record(pdu, {
    last_seq :: last_seq()
}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_heartbeat(PDU :: pdu_internal()).



%% Interface



-spec new(LastSeq :: last_seq()) ->
    Ret :: pdu().

new(LastSeq) ->
    ?marvin_pdu_heartbeat(#pdu{
        last_seq = LastSeq
    }).



-spec data_map() ->
    jiffy_vm:jv_type_integer().

data_map() ->
    jiffy_vm:integer(fun validate_last_seq/3).



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: last_seq()).

export(?marvin_pdu_heartbeat(#pdu{
    last_seq = LastSeq
})) ->
    {ok, LastSeq}.



-spec last_seq(PDU :: pdu()) ->
    Ret :: last_seq().

last_seq(?marvin_pdu_heartbeat(#pdu{last_seq = Value})) ->
    Value.



-spec last_seq(
    LastSeq :: last_seq(),
    PDU :: pdu()
) ->
    Ret :: pdu().

last_seq(Value, ?marvin_pdu_heartbeat(#pdu{} = PDU)) ->
    ?marvin_pdu_heartbeat(PDU#pdu{last_seq = Value}).



%% Internals



-spec ?validator_spec(validate_last_seq).

validate_last_seq(validate, _, Value)
when Value < 0 ->
    {error, <<"Should be positive">>};

validate_last_seq(validate, _, _Value) ->
    {ok, valid};

validate_last_seq(fix, _, _) ->
    {error, invalid}.
