-module(marvin_pdu2_object_user).
-compile({parse_transform, cloak_transform}).

-export([export/1, is/2, format/1, format_username/1]).

-record(?MODULE, {
    id = undefined :: id(),
    username = undefined :: username(),
    discriminator = undefined :: discriminator(),
    avatar = undefined :: avatar() | undefined,
    bot = false :: bot(),
    mfa_enabled = false :: mfa_enabled(),
    verified = false :: verified(),
    email = undefined :: email()
}).

-type id() :: marvin_pdu2:snowflake().
-type username() :: unicode:unicode_binary().
-type discriminator() :: unicode:unicode_binary().
-type avatar() :: unicode:unicode_binary().
-type bot() :: boolean().
-type mfa_enabled() :: boolean().
-type verified() :: boolean().
-type email() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([
    id/0, username/0, discriminator/0, avatar/0, bot/0,
    mfa_enabled/0, verified/0, email/0, t/0
]).


is(#?MODULE{id = _UserId}, _UserId) -> true;
is(#?MODULE{id = _AnotherUserId}, _UserId) -> false.


format(#?MODULE{id = Id}) ->
    <<"<@", Id/binary, ">">>.


format_username(#?MODULE{id = Id}) ->
    <<"<@!", Id/binary, ">">>.


cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    id = Id,
    username = Username,
    discriminator = Discriminator,
    avatar = Avatar,
    bot = Bot,
    mfa_enabled = MfaEnabled,
    verified = Verified,
    email = Email
}) ->
    #{
        <<"id">> => Id,
        <<"username">> => Username,
        <<"discriminator">> => Discriminator,
        <<"avatar">> => marvin_pdu2:nullify(Avatar),
        <<"bot">> => Bot,
        <<"mfa_enabled">> => MfaEnabled,
        <<"verified">> => Verified,
        <<"email">> => marvin_pdu2:nullify(Email)
    }.
