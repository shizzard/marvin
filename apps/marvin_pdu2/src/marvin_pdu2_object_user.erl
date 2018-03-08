-module(marvin_pdu2_object_user).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: marvin_pdu2:snowflake(),
    username :: marvin_pdu2:username(),
    discriminator :: marvin_pdu2:discriminator(),
    avatar :: marvin_pdu2:avatar(),
    bot = false :: marvin_pdu2:bot(),
    mfa_enabled = false :: marvin_pdu2:mfa_enabled(),
    verified = false :: marvin_pdu2:verified(),
    email = undefined :: marvin_pdu2:email()
}).

-type t() :: #?MODULE{}.
-export_type([t/0]).


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(username, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(discriminator, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

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
        <<"avatar">> => Avatar,
        <<"bot">> => Bot,
        <<"mfa_enabled">> => MfaEnabled,
        <<"verified">> => Verified,
        <<"email">> => Email
    }.
