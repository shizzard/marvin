-module(marvin_pdu2_resume).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    token = undefined :: token(),
    session_id = undefined :: session_id(),
    seq = undefined :: sequence()
}).

-type token() :: marvin_pdu2:token().
-type session_id() :: marvin_pdu2:session_id().
-type sequence() :: marvin_pdu2:sequence().
-type t() :: #?MODULE{}.

-export_type([token/0, session_id/0, sequence/0, t/0]).

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    token = Token,
    session_id = SessionId,
    seq = Sequence
}) ->
    #{
        <<"token">> => Token,
        <<"session_id">> => SessionId,
        <<"seq">> => Sequence
    }.
