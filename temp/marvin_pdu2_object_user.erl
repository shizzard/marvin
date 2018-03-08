-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_object_user.erl",
      1).
-module(marvin_pdu2_object_user).
-export([export/1]).
-record(marvin_pdu2_object_user,{id :: marvin_pdu2:snowflake(),
                                 username :: marvin_pdu2:username(),
                                 discriminator ::
                                     marvin_pdu2:discriminator(),
                                 avatar :: marvin_pdu2:avatar(),
                                 bot = false :: marvin_pdu2:bot(),
                                 mfa_enabled =
                                     false :: marvin_pdu2:mfa_enabled(),
                                 verified =
                                     false :: marvin_pdu2:verified(),
                                 email =
                                     undefined :: marvin_pdu2:email()}).
-type t() :: #marvin_pdu2_object_user{}.
-export_type([t/0]).
-export([bot/2,
         mfa_enabled/2,
         verified/2,
         email/2,
         id/2,
         username/2,
         discriminator/2,
         avatar/2,
         bot/1,
         mfa_enabled/1,
         verified/1,
         email/1,
         id/1,
         username/1,
         discriminator/1,
         avatar/1,
         update/2,
         new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_object_user{},
                                                        [{avatar,
                                                          <<97,
                                                            118,
                                                            97,
                                                            116,
                                                            97,
                                                            114>>},
                                                         {discriminator,
                                                          <<100,
                                                            105,
                                                            115,
                                                            99,
                                                            114,
                                                            105,
                                                            109,
                                                            105,
                                                            110,
                                                            97,
                                                            116,
                                                            111,
                                                            114>>},
                                                         {username,
                                                          <<117,
                                                            115,
                                                            101,
                                                            114,
                                                            110,
                                                            97,
                                                            109,
                                                            101>>},
                                                         {id,
                                                          <<105,100>>}]),
                                           [{email,
                                             <<101,109,97,105,108>>},
                                            {verified,
                                             <<118,
                                               101,
                                               114,
                                               105,
                                               102,
                                               105,
                                               101,
                                               100>>},
                                            {mfa_enabled,
                                             <<109,
                                               102,
                                               97,
                                               95,
                                               101,
                                               110,
                                               97,
                                               98,
                                               108,
                                               101,
                                               100>>},
                                            {bot,<<98,111,116>>}]))
    of
        {ok,Var_value_1} ->
            Var_value_1;
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: struct validation fai"
                                   "led with reason: ~p",
                                   [Var_reason_0]),
            error(badarg)
    end;
new(_) ->
    error(badarg).
required_new(_, Var_record_0, []) ->
    Var_record_0;
required_new(Var_map_0,
             Var_record_0,
             [{Var_key_0,Var_binkey_0}|Var_keys_0]) ->
    case
        {maps:is_key(Var_key_0, Var_map_0),
         maps:is_key(Var_binkey_0, Var_map_0)}
    of
        {true,_} ->
            required_new(Var_map_0,
                         marvin_pdu2_object_user:Var_key_0(Var_record_0,
                                                           maps:get(Var_key_0,
                                                                    Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_object_user:Var_key_0(Var_record_0,
                                                           maps:get(Var_binkey_0,
                                                                    Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            error_logger:error_msg("cloak badarg: required field '~s' i"
                                   "s not found",
                                   [Var_key_0]),
            error(badarg)
    end.
optional_new(_, Var_record_0, []) ->
    Var_record_0;
optional_new(Var_map_0,
             Var_record_0,
             [{Var_key_0,Var_binkey_0}|Var_keys_0]) ->
    case
        {maps:is_key(Var_key_0, Var_map_0),
         maps:is_key(Var_binkey_0, Var_map_0)}
    of
        {true,_} ->
            optional_new(Var_map_0,
                         marvin_pdu2_object_user:Var_key_0(Var_record_0,
                                                           maps:get(Var_key_0,
                                                                    Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_object_user:Var_key_0(Var_record_0,
                                                           maps:get(Var_binkey_0,
                                                                    Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_object_user{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{avatar,
                                             <<97,118,97,116,97,114>>},
                                            {discriminator,
                                             <<100,
                                               105,
                                               115,
                                               99,
                                               114,
                                               105,
                                               109,
                                               105,
                                               110,
                                               97,
                                               116,
                                               111,
                                               114>>},
                                            {username,
                                             <<117,
                                               115,
                                               101,
                                               114,
                                               110,
                                               97,
                                               109,
                                               101>>},
                                            {id,<<105,100>>},
                                            {email,
                                             <<101,109,97,105,108>>},
                                            {verified,
                                             <<118,
                                               101,
                                               114,
                                               105,
                                               102,
                                               105,
                                               101,
                                               100>>},
                                            {mfa_enabled,
                                             <<109,
                                               102,
                                               97,
                                               95,
                                               101,
                                               110,
                                               97,
                                               98,
                                               108,
                                               101,
                                               100>>},
                                            {bot,<<98,111,116>>}]))
    of
        {ok,Var_value_1} ->
            Var_value_1;
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: struct validation fai"
                                   "led with reason: ~p",
                                   [Var_reason_0]),
            error(badarg)
    end;
update(_, _) ->
    error(badarg).
avatar(#marvin_pdu2_object_user{avatar = Var_avatar_0}) ->
    Var_avatar_0;
avatar(_) ->
    error(badarg).
discriminator(#marvin_pdu2_object_user{discriminator =
                                           Var_discriminator_0}) ->
    Var_discriminator_0;
discriminator(_) ->
    error(badarg).
username(#marvin_pdu2_object_user{username = Var_username_0}) ->
    Var_username_0;
username(_) ->
    error(badarg).
id(#marvin_pdu2_object_user{id = Var_id_0}) ->
    Var_id_0;
id(_) ->
    error(badarg).
email(#marvin_pdu2_object_user{email = Var_email_0}) ->
    Var_email_0;
email(_) ->
    error(badarg).
verified(#marvin_pdu2_object_user{verified = Var_verified_0}) ->
    Var_verified_0;
verified(_) ->
    error(badarg).
mfa_enabled(#marvin_pdu2_object_user{mfa_enabled = Var_mfa_enabled_0}) ->
    Var_mfa_enabled_0;
mfa_enabled(_) ->
    error(badarg).
bot(#marvin_pdu2_object_user{bot = Var_bot_0}) ->
    Var_bot_0;
bot(_) ->
    error(badarg).
avatar(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(avatar, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(avatar,
                          Var_record_0#marvin_pdu2_object_user{avatar =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [avatar,Var_reason_0]),
            error(badarg)
    end;
avatar(_, _) ->
    error(badarg).
discriminator(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(discriminator, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(discriminator,
                          Var_record_0#marvin_pdu2_object_user{discriminator =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [discriminator,Var_reason_0]),
            error(badarg)
    end;
discriminator(_, _) ->
    error(badarg).
username(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(username, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(username,
                          Var_record_0#marvin_pdu2_object_user{username =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [username,Var_reason_0]),
            error(badarg)
    end;
username(_, _) ->
    error(badarg).
id(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(id, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(id,
                          Var_record_0#marvin_pdu2_object_user{id =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [id,Var_reason_0]),
            error(badarg)
    end;
id(_, _) ->
    error(badarg).
email(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(email, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(email,
                          Var_record_0#marvin_pdu2_object_user{email =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [email,Var_reason_0]),
            error(badarg)
    end;
email(_, _) ->
    error(badarg).
verified(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(verified, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(verified,
                          Var_record_0#marvin_pdu2_object_user{verified =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [verified,Var_reason_0]),
            error(badarg)
    end;
verified(_, _) ->
    error(badarg).
mfa_enabled(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(mfa_enabled, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(mfa_enabled,
                          Var_record_0#marvin_pdu2_object_user{mfa_enabled =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [mfa_enabled,Var_reason_0]),
            error(badarg)
    end;
mfa_enabled(_, _) ->
    error(badarg).
bot(#marvin_pdu2_object_user{} = Var_record_0, Var_value_0) ->
    case cloak_validate(bot, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(bot,
                          Var_record_0#marvin_pdu2_object_user{bot =
                                                                   Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [bot,Var_reason_0]),
            error(badarg)
    end;
bot(_, _) ->
    error(badarg).
cloak_validate_struct(Var_value_0) ->
    {ok,Var_value_0}.
cloak_updated(_, Var_record_0) ->
    Var_record_0.

cloak_validate(id, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(username, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(discriminator, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(avatar, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(bot, Value) when is_boolean(Value) ->
    {ok,Value};
cloak_validate(mfa_enabled, Value) when is_boolean(Value) ->
    {ok,Value};
cloak_validate(verified, Value) when is_boolean(Value) ->
    {ok,Value};
cloak_validate(email, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(email, null) ->
    {ok,undefined};
cloak_validate(_, _) ->
    {error,invalid}.
export(#marvin_pdu2_object_user{id = Id,
                                username = Username,
                                discriminator = Discriminator,
                                avatar = Avatar,
                                bot = Bot,
                                mfa_enabled = MfaEnabled,
                                verified = Verified,
                                email = Email}) ->
    #{<<"id">> => Id,
      <<"username">> => Username,
      <<"discriminator">> => Discriminator,
      <<"avatar">> => Avatar,
      <<"bot">> => Bot,
      <<"mfa_enabled">> => MfaEnabled,
      <<"verified">> => Verified,
      <<"email">> => Email}.

