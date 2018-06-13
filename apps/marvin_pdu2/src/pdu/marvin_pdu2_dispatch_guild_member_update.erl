-module(marvin_pdu2_dispatch_guild_member_update).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guild_id :: guild_id(),
    roles :: roles(),
    user :: user(),
    nick = undefined :: nick()
}).

-type guild_id() :: marvin_pdu2:snowflake().
-type roles() :: [marvin_pdu2:snowflake()].
-type user() :: marvin_pdu2_object_user:t().
-type nick() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([guild_id/0, roles/0, user/0, nick/0, t/0]).


cloak_validate(guild_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(roles, Value) when is_list(Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(user, Value) ->
    {ok, marvin_pdu2_object_user:new(Value)};

cloak_validate(nick, null) ->
    {ok, undefined};

cloak_validate(nick, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    guild_id = GuildId,
    roles = Roles,
    user = User,
    nick = Nick
}) ->
    #{
        <<"guild_id">> => GuildId,
        <<"roles">> => Roles,
        <<"user">> => marvin_pdu2_object_user:export(User),
        <<"nick">> => marvin_pdu2:nullify(Nick)
    }.
