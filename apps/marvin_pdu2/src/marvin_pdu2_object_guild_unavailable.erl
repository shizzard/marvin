-module(marvin_pdu2_object_guild_unavailable).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: id(),
    unavailable :: unavailable()
}).

-type id() :: marvin_pdu2:snowflake().
-type unavailable() :: boolean().
-type t() :: #?MODULE{}.

-export_type([id/0, unavailable/0, t/0]).


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(unavailable, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    id = Id,
    unavailable = Unavailable
}) ->
    #{
        <<"id">> => Id,
        <<"unavailable">> => Unavailable
    }.
