-module(marvin_pdu2_object_guild_unavailable).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id = undefined :: id(),
    unavailable = undefined :: unavailable()
}).

-type id() :: marvin_pdu2:snowflake().
-type unavailable() :: boolean().
-type t() :: #?MODULE{}.

-export_type([id/0, unavailable/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    id = Id,
    unavailable = Unavailable
}) ->
    #{
        <<"id">> => marvin_pdu2:nullify(Id),
        <<"unavailable">> => marvin_pdu2:nullify(Unavailable)
    }.
