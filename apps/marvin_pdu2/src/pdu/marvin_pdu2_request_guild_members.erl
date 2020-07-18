-module(marvin_pdu2_request_guild_members).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guild_id = undefined :: guild_id(),
    query = undefined :: query(),
    limit = undefined :: limit()
}).

-type guild_id() :: marvin_pdu2:snowflake().
-type query() :: unicode:unicode_binary().
-type limit() :: non_neg_integer().
-type t() :: #?MODULE{}.

-export_type([guild_id/0, 'query'/0, limit/0, t/0]).

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    guild_id = GuildId,
    query = Query,
    limit = Limit
}) ->
    #{
        <<"guild_id">> => marvin_pdu2:nullify(GuildId),
        <<"query">> => marvin_pdu2:nullify(Query),
        <<"limit">> => marvin_pdu2:nullify(Limit)
    }.
