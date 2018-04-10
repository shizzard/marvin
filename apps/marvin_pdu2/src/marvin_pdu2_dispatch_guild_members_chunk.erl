-module(marvin_pdu2_dispatch_guild_members_chunk).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guild_id :: guild_id(),
    members = [] :: members()
}).

-type guild_id() :: marvin_pdu2:snowflake().
-type members() :: [marvin_pdu2_object_member:t()].
-type t() :: #?MODULE{}.

-export_type([guild_id/0, members/0, t/0
]).


cloak_validate(guild_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(members, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_member:new(Item) || Item <- Value]};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    guild_id = GuildId,
    members = Members
}) ->
    #{
        <<"guild_id">> => GuildId,
        <<"members">> => [marvin_pdu2_object_member:export(Item) || Item <- Members]
    }.
