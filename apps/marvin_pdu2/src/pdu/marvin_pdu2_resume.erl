-module(marvin_pdu2_resume).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    token :: token(),
    session_id :: session_id(),
    seq :: sequence()
}).

-type token() :: marvin_pdu2:token().
-type session_id() :: marvin_pdu2:session_id().
-type sequence() :: marvin_pdu2:sequence().
-type t() :: #?MODULE{}.

-export_type([token/0, session_id/0, sequence/0, t/0]).


cloak_validate(token, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(session_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(seq, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


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
