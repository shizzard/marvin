-module(marvin_pdu2_object_member).
-compile({parse_transform, cloak_transform}).

-export([export/1, get_nickname/1, have_role/2]).

-record(?MODULE, {
    user = undefined :: user(),
    roles = [] :: roles(),
    nick = undefined :: nick() | undefined,
    mute = undefined :: mute(),
    joined_at = undefined :: joined_at(),
    deaf = undefined :: deaf()
}).

-type user() :: marvin_pdu2_object_user:t().
-type roles() :: [marvin_pdu2_object_role:id()].
-type nick() :: unicode:unicode_binary().
-type mute() :: boolean().
-type joined_at() :: unicode:unicode_binary().
-type deaf() :: boolean().
-type t() :: #?MODULE{}.

-export_type([user/0, roles/0, nick/0, mute/0, joined_at/0, deaf/0, t/0]).



get_nickname(#?MODULE{nick = undefined, user = User}) ->
    marvin_pdu2_object_user:username(User);

get_nickname(#?MODULE{nick = Nickname}) ->
    Nickname.



have_role(Member, RoleId) ->
    lists:member(RoleId, roles(Member)).


cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(user, Value) ->
    {ok, marvin_pdu2_object_user:new(Value)};

cloak_validate(_, Value) ->
    {ok, Value}.


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
        <<"roles">> => marvin_pdu2:nullify(Roles),
        <<"nick">> => marvin_pdu2:nullify(Nick),
        <<"mute">> => marvin_pdu2:nullify(Mute),
        <<"joined_at">> => marvin_pdu2:nullify(JoinedAt),
        <<"deaf">> => marvin_pdu2:nullify(Deaf)
    }.
