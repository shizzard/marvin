-module(marvin_pdu2_request_guild_members).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guild_id :: guild_id(),
    query :: query(),
    limit :: limit()
}).

-type guild_id() :: marvin_pdu2:snowflake().
-type query() :: unicode:unicode_binary().
-type limit() :: non_neg_integer().
-type t() :: #?MODULE{}.

-export_type([guild_id/0, 'query'/0, limit/0, t/0]).


cloak_validate(guild_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(query, Value) when is_binary(Value) ->
    {ok, Value};

cloak_validate(limit, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    guild_id = GuildId,
    query = Query,
    limit = Limit
}) ->
    #{
        <<"guild_id">> => GuildId,
        <<"query">> => Query,
        <<"limit">> => Limit
    }.
