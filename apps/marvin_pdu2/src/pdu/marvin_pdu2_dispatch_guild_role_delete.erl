-module(marvin_pdu2_dispatch_guild_role_delete).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guild_id :: guild_id(),
    role_id :: role_id()
}).

-type guild_id() :: marvin_pdu2:snowflake().
-type role_id() :: marvin_pdu2:snowflake().
-type t() :: #?MODULE{}.

-export_type([guild_id/0, role_id/0, t/0]).


cloak_validate(guild_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(role_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    guild_id = GuildId,
    role_id = RoleId
}) ->
    #{
        <<"guild_id">> => GuildId,
        <<"role_id">> => RoleId
    }.
