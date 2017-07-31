-module(marvin_pdu_object_user).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu_object.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, new/8, data_map/0, export/1]).
-export([
    id/1, username/1, discriminator/1, avatar/1, bot/1,
    mfa_enabled/1, verified/1, email/1,
    id/2, username/2, discriminator/2, avatar/2, bot/2,
    mfa_enabled/2, verified/2, email/2
]).



%% Types



-type id() :: non_neg_integer().
-type username() :: binary().
-type discriminator() :: binary().
-type avatar() :: binary().
-type bot() :: boolean().
-type mfa_enabled() :: boolean().
-type verified() :: boolean().
-type email() :: binary().

-export_type([
    id/0, username/0, discriminator/0, avatar/0, bot/0,
    mfa_enabled/0, verified/0, email/0
]).

-record(object, {
    id :: id(),
    username :: username(),
    discriminator :: discriminator(),
    avatar :: avatar(),
    bot :: bot(),
    mfa_enabled :: mfa_enabled(),
    verified :: verified(),
    email :: email()
}).
-type object_internal() :: #object{}.
-type object() :: ?marvin_pdu_object_user(PDU :: object_internal()).



%% Interface



-spec new(Data :: #{}) ->
    Ret :: object().

new(#{
    ?discord_key_object_user_id := Id,
    ?discord_key_object_user_username := Username,
    ?discord_key_object_user_discriminator := Discriminator,
    ?discord_key_object_user_avatar := Avatar,
    ?discord_key_object_user_bot := Bot,
    ?discord_key_object_user_mfa_enabled := MfaEnabled,
    ?discord_key_object_user_verified := Verified,
    ?discord_key_object_user_email := Email
}) ->
    new(Id, Username, Discriminator, Avatar, Bot, MfaEnabled, Verified, Email).



-spec new(
    Id :: id(),
    Username :: username(),
    Discriminator :: discriminator(),
    Avatar :: avatar(),
    Bot :: bot(),
    MfaEnabled :: mfa_enabled(),
    Verified :: verified(),
    Email :: email()
) ->
    Ret :: object().

new(Id, Username, Discriminator, Avatar, Bot, MfaEnabled, Verified, Email) ->
    ?marvin_pdu_object_user(#object{
        id = Id,
        username = Username,
        discriminator = Discriminator,
        avatar = Avatar,
        bot = Bot,
        mfa_enabled = MfaEnabled,
        verified = Verified,
        email = Email
    }).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_object_user_id, required, jiffy_vm:integer(fun validate_id/3)),
        jiffy_vm:hashfield(?discord_key_object_user_username, required, jiffy_vm:string()),
        jiffy_vm:hashfield(?discord_key_object_user_discriminator, required, jiffy_vm:string()),
        jiffy_vm:hashfield(?discord_key_object_user_avatar, required, jiffy_vm:string()),
        jiffy_vm:hashfield(?discord_key_object_user_bot, required, jiffy_vm:boolean(), fun validate_booleans_default_false/3),
        jiffy_vm:hashfield(?discord_key_object_user_mfa_enabled, required, jiffy_vm:boolean(), fun validate_booleans_default_false/3),
        jiffy_vm:hashfield(?discord_key_object_user_verified, required, jiffy_vm:boolean(), fun validate_booleans_default_false/3),
        jiffy_vm:hashfield(?discord_key_object_user_email, required, jiffy_vm:string())
    ]).



-spec export(PDU :: object()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_object_user(#object{
    id = Id,
    username = Username,
    discriminator = Discriminator,
    avatar = Avatar,
    bot = Bot,
    mfa_enabled = MfaEnabled,
    verified = Verified,
    email = Email
})) ->
    {ok, #{
        ?discord_key_object_user_id => Id,
        ?discord_key_object_user_username => Username,
        ?discord_key_object_user_discriminator => Discriminator,
        ?discord_key_object_user_avatar => Avatar,
        ?discord_key_object_user_bot => Bot,
        ?discord_key_object_user_mfa_enabled => MfaEnabled,
        ?discord_key_object_user_verified => Verified,
        ?discord_key_object_user_email => Email
    }}.



-spec id(Object :: object()) ->
    Ret :: id().

id(?marvin_pdu_object_user(#object{id = Value})) ->
    Value.



-spec id(Value :: id(), Object :: object()) ->
    Ret :: object().

id(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{id = Value}).



-spec username(Object :: object()) ->
    Ret :: username().

username(?marvin_pdu_object_user(#object{username = Value})) ->
    Value.



-spec username(Value :: username(), Object :: object()) ->
    Ret :: object().

username(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{username = Value}).



-spec discriminator(Object :: object()) ->
    Ret :: discriminator().

discriminator(?marvin_pdu_object_user(#object{discriminator = Value})) ->
    Value.



-spec discriminator(Value :: discriminator(), Object :: object()) ->
    Ret :: object().

discriminator(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{discriminator = Value}).



-spec avatar(Object :: object()) ->
    Ret :: avatar().

avatar(?marvin_pdu_object_user(#object{avatar = Value})) ->
    Value.



-spec avatar(Value :: avatar(), Object :: object()) ->
    Ret :: object().

avatar(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{avatar = Value}).



-spec bot(Object :: object()) ->
    Ret :: bot().

bot(?marvin_pdu_object_user(#object{bot = Value})) ->
    Value.



-spec bot(Value :: bot(), Object :: object()) ->
    Ret :: object().

bot(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{bot = Value}).



-spec mfa_enabled(Object :: object()) ->
    Ret :: mfa_enabled().

mfa_enabled(?marvin_pdu_object_user(#object{mfa_enabled = Value})) ->
    Value.



-spec mfa_enabled(Value :: mfa_enabled(), Object :: object()) ->
    Ret :: object().

mfa_enabled(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{mfa_enabled = Value}).



-spec verified(Object :: object()) ->
    Ret :: verified().

verified(?marvin_pdu_object_user(#object{verified = Value})) ->
    Value.



-spec verified(Value :: verified(), Object :: object()) ->
    Ret :: object().

verified(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{verified = Value}).



-spec email(Object :: object()) ->
    Ret :: email().

email(?marvin_pdu_object_user(#object{email = Value})) ->
    Value.



-spec email(Value :: email(), Object :: object()) ->
    Ret :: object().

email(Value, ?marvin_pdu_object_user(#object{} = Object)) ->
    ?marvin_pdu_object_user(Object#object{email = Value}).



%% Internals



-spec ?validator_spec(validate_id).

validate_id(validate, _, _Id) ->
    {ok, valid};

validate_id(fix, _, Id) when is_binary(Id) ->
    {ok, binary_to_integer(Id)}.



-spec ?validator_spec(validate_booleans_default_false).

validate_booleans_default_false(validate, _, _) ->
    {ok, valid};

validate_booleans_default_false(fix, _, _) ->
    {ok, false}.

