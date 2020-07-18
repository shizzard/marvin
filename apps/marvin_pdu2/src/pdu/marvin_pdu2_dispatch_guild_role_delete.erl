-module(marvin_pdu2_dispatch_guild_role_delete).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guild_id = undefined :: guild_id(),
    role_id = undefined :: role_id()
}).

-type guild_id() :: marvin_pdu2:snowflake().
-type role_id() :: marvin_pdu2:snowflake().
-type t() :: #?MODULE{}.

-export_type([guild_id/0, role_id/0, t/0]).

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    guild_id = GuildId,
    role_id = RoleId
}) ->
    #{
        <<"guild_id">> => GuildId,
        <<"role_id">> => RoleId
    }.
