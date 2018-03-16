-module(marvin_pdu2_object_member).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    user :: user(),
    roles :: roles(),
    nick = undefined :: nick() | undefined,
    mute :: mute(),
    joined_at :: joined_at(),
    deaf :: deaf()
}).

-type user() :: marvin_pdu2_object_user:t().
-type roles() :: [marvin_pdu2_object_role:id()].
-type nick() :: unicode:unicode_binary().
-type mute() :: boolean().
-type joined_at() :: unicode:unicode_binary().
-type deaf() :: boolean().
-type t() :: #?MODULE{}.

-export_type([user/0, roles/0, nick/0, mute/0, joined_at/0, deaf/0, t/0]).


cloak_validate(user, Value) ->
    {ok, marvin_pdu2_object_user:new(Value)};

cloak_validate(roles, Value) when is_list(Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(nick, null) ->
    {ok, undefined};

cloak_validate(nick, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(mute, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(joined_at, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(deaf, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    user = User,
    roles = Roles,
    nick = Nick,
    mute = Mute,
    joined_at = JoinedAt,
    deaf = Deaf
}) ->
    #{
        <<"user">> => marvin_pdu2_object_user:export(User),
        <<"roles">> => Roles,
        <<"nick">> => marvin_pdu2:nullify(Nick),
        <<"mute">> => Mute,
        <<"joined_at">> => JoinedAt,
        <<"deaf">> => Deaf
    }.
