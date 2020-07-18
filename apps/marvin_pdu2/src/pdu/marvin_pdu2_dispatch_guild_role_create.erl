-module(marvin_pdu2_dispatch_guild_role_create).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guild_id = undefined :: guild_id(),
    role = undefined :: role()
}).

-type guild_id() :: marvin_pdu2:snowflake().
-type role() :: marvin_pdu2_object_role:t().
-type t() :: #?MODULE{}.

-export_type([guild_id/0, role/0, t/0]).

cloak_validate(role, Value) ->
    {ok, marvin_pdu2_object_role:new(Value)};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    guild_id = GuildId,
    role = Role
}) ->
    #{
        <<"guild_id">> => marvin_pdu2:nullify(GuildId),
        <<"role">> => marvin_pdu2_object_role:export(Role)
    }.
