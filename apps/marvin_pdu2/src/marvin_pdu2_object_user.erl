-module(marvin_pdu2_object_user).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: id(),
    username :: username(),
    discriminator :: discriminator(),
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


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(username, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(discriminator, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(avatar, null) ->
    {ok, undefined};

cloak_validate(avatar, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(bot, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(mfa_enabled, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(verified, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(email, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(email, null) ->
    {ok, undefined};

cloak_validate(_, _) ->
    {error, invalid}.


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
