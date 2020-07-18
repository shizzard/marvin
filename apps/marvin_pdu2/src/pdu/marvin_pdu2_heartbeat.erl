-module(marvin_pdu2_heartbeat).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    plain_value = undefined :: sequence()
}).

-type sequence() :: marvin_pdu2:sequence().
-type t() :: #?MODULE{}.

-export_type([sequence/0, t/0]).


cloak_validate(plain_value, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{plain_value = LastSeq}) ->
    #{<<"plain_value">> => LastSeq}.
