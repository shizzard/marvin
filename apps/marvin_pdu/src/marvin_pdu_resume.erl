-module(marvin_pdu_resume).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, new/3, data_map/0, export/1]).
-export([token/1, token/2, session_id/1, session_id/2, seq/1, seq/2]).



%% Types



-type token() :: binary().
-type session_id() :: binary().
-type seq() :: non_neg_integer().
-export_type([token/0, session_id/0, seq/0]).

-record(pdu, {
    token :: token(),
    session_id :: session_id(),
    seq :: seq()
}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_resume(PDU :: pdu_internal()).



%% Interface



-spec new(Data :: #{}) ->
    Ret :: pdu().

new(#{
    ?discord_key_resume_token := Token,
    ?discord_key_resume_session_id := SessionId,
    ?discord_key_resume_seq := Seq
}) ->
    new(Token, SessionId, Seq).



-spec new(
    Token :: token(),
    SessionId :: session_id(),
    Seq :: seq()
) ->
    Ret :: pdu().

new(Token, SessionId, Seq) ->
    ?marvin_pdu_resume(#pdu{
        token = Token,
        session_id = SessionId,
        seq = Seq
    }).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_resume_token, required, jiffy_vm:string()),
        jiffy_vm:hashfield(?discord_key_resume_session_id, required, jiffy_vm:string()),
        jiffy_vm:hashfield(?discord_key_resume_seq, required, jiffy_vm:integer())
    ]).



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_resume(#pdu{
    token = Token,
    session_id = SessionId,
    seq = Seq
})) ->
    {ok, #{
        ?discord_key_resume_token => Token,
        ?discord_key_resume_session_id => SessionId,
        ?discord_key_resume_seq => Seq
    }}.



-spec token(PDU :: pdu()) ->
    Ret :: token().

token(?marvin_pdu_resume(#pdu{token = Value})) ->
    Value.



-spec token(OS :: token(), PDU :: pdu()) ->
    Ret :: pdu().

token(Value, ?marvin_pdu_resume(#pdu{} = PDU)) ->
    ?marvin_pdu_resume(PDU#pdu{token = Value}).



-spec session_id(PDU :: pdu()) ->
    Ret :: session_id().

session_id(?marvin_pdu_resume(#pdu{session_id = Value})) ->
    Value.



-spec session_id(OS :: session_id(), PDU :: pdu()) ->
    Ret :: pdu().

session_id(Value, ?marvin_pdu_resume(#pdu{} = PDU)) ->
    ?marvin_pdu_resume(PDU#pdu{session_id = Value}).



-spec seq(PDU :: pdu()) ->
    Ret :: seq().

seq(?marvin_pdu_resume(#pdu{seq = Value})) ->
    Value.



-spec seq(OS :: seq(), PDU :: pdu()) ->
    Ret :: pdu().

seq(Value, ?marvin_pdu_resume(#pdu{} = PDU)) ->
    ?marvin_pdu_resume(PDU#pdu{seq = Value}).



%% Internals
