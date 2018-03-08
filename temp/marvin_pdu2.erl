-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2.erl",
      1).
-module(marvin_pdu2).
-record(marvin_pdu2,{op :: operation(),
                     d = #{} :: data(),
                     s = undefined :: sequence(),
                     t = undefined :: event(),
                     prot_mod :: atom()}).
-export([parse/1,decode_json/1,construct_pdu2/1]).
-type t() :: #marvin_pdu2{}.
-type operation() :: 0..11.
-type data() :: term().
-type sequence() :: pos_integer().
-type event() :: binary().
-type heartbeat_interval() :: pos_integer().
-type trace_part() :: binary().
-type trace() :: [trace_part(), ...].
-type token() :: binary().
-type compress() :: boolean().
-type large_threshold() :: 50..250.
-type properties_os() :: binary().
-type properties_browser() :: binary().
-type properties_device() :: binary().
-type properties_referrer() :: binary().
-type properties_referring_domain() :: binary().
-type shard() :: non_neg_integer().
-type total_shards() :: pos_integer().
-type shard_spec() :: [shard() | total_shards()].
-type session_id() :: binary().
-type protocol_version() :: pos_integer().
-type snowflake() :: binary().
-type username() :: binary().
-type discriminator() :: binary().
-type avatar() :: binary().
-type bot() :: boolean().
-type mfa_enabled() :: boolean().
-type verified() :: boolean().
-type email() :: binary().
-type unavailable() :: boolean().
-export_type([operation/0,
              data/0,
              sequence/0,
              event/0,
              heartbeat_interval/0,
              trace_part/0,
              trace/0,
              token/0,
              compress/0,
              large_threshold/0,
              properties_os/0,
              properties_browser/0,
              properties_device/0,
              properties_referrer/0,
              properties_referring_domain/0,
              shard/0,
              total_shards/0,
              shard_spec/0,
              session_id/0,
              protocol_version/0,
              snowflake/0,
              username/0,
              discriminator/0,
              avatar/0,
              bot/0,
              mfa_enabled/0,
              verified/0,
              email/0,
              unavailable/0]).
-spec parse(Binary :: binary()) ->
               marvin_helper_type:generic_return(OkRet :: t(),
                                                 ErrorRet ::
                                                     {jiffy_error,
                                                      term()} |
                                                     not_implemented |
                                                     invalid_op |
                                                     term()).
-export([d/2,s/2,t/2,op/2,prot_mod/1,d/1,s/1,t/1,op/1,update/2,new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2{},
                                                        [{op,
                                                          <<111,112>>}]),
                                           [{t,<<116>>},
                                            {s,<<115>>},
                                            {d,<<100>>}]))
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
                         marvin_pdu2:Var_key_0(Var_record_0,
                                               maps:get(Var_key_0,
                                                        Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2:Var_key_0(Var_record_0,
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
                         marvin_pdu2:Var_key_0(Var_record_0,
                                               maps:get(Var_key_0,
                                                        Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2:Var_key_0(Var_record_0,
                                               maps:get(Var_binkey_0,
                                                        Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{op,<<111,112>>},
                                            {t,<<116>>},
                                            {s,<<115>>},
                                            {d,<<100>>}]))
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
op(#marvin_pdu2{op = Var_op_0}) ->
    Var_op_0;
op(_) ->
    error(badarg).
t(#marvin_pdu2{t = Var_t_0}) ->
    Var_t_0;
t(_) ->
    error(badarg).
s(#marvin_pdu2{s = Var_s_0}) ->
    Var_s_0;
s(_) ->
    error(badarg).
d(#marvin_pdu2{d = Var_d_0}) ->
    Var_d_0;
d(_) ->
    error(badarg).
prot_mod(#marvin_pdu2{prot_mod = Var_prot_mod_0}) ->
    Var_prot_mod_0;
prot_mod(_) ->
    error(badarg).
op(#marvin_pdu2{} = Var_record_0, Var_value_0) ->
    case cloak_validate(op, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(op,
                          Var_record_0#marvin_pdu2{op = Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [op,Var_reason_0]),
            error(badarg)
    end;
op(_, _) ->
    error(badarg).
t(#marvin_pdu2{} = Var_record_0, Var_value_0) ->
    case cloak_validate(t, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(t, Var_record_0#marvin_pdu2{t = Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [t,Var_reason_0]),
            error(badarg)
    end;
t(_, _) ->
    error(badarg).
s(#marvin_pdu2{} = Var_record_0, Var_value_0) ->
    case cloak_validate(s, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(s, Var_record_0#marvin_pdu2{s = Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [s,Var_reason_0]),
            error(badarg)
    end;
s(_, _) ->
    error(badarg).
d(#marvin_pdu2{} = Var_record_0, Var_value_0) ->
    case cloak_validate(d, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(d, Var_record_0#marvin_pdu2{d = Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [d,Var_reason_0]),
            error(badarg)
    end;
d(_, _) ->
    error(badarg).
cloak_updated(_, Var_record_0) ->
    Var_record_0.

parse(Binary) ->
    marvin_helper_chain:chain('marvin_pdu2:parse',
                              [fun decode_json/1,fun construct_pdu2/1],
                              Binary).
-spec decode_json(Binary :: binary()) ->
                     marvin_helper_type:generic_return(OkRet :: #{},
                                                       ErrorRet ::
                                                           {jiffy_error,
                                                            term()}).
decode_json(Binary) ->
    try
        Message = jiffy:decode(Binary, [return_maps]),
        {ok,Message}
    catch
        _Type:Error ->
            {error,{jiffy_error,Error}}
    end.
-spec construct_pdu2(Message :: #{}) ->
                        marvin_helper_type:ok_return(OkRet :: t()) |
                        no_return().
construct_pdu2(Message) ->
    {ok,marvin_pdu2:new(Message)}.
cloak_validate(op, Value) ->
    case lists:member(Value, lists:seq(0, 11)) of
        true ->
            {ok,Value};
        false ->
            {error,invalid}
    end;
cloak_validate(d, Value)
    when
        null == Value
        orelse
        undefined == Value
        orelse
        false == Value ->
    {ok,#{}};
cloak_validate(d, Value) ->
    case Value of
        _ when is_map(Value) ->
            {ok,Value};
        _ ->
            {ok,#{<<"plain_value">> => Value}}
    end;
cloak_validate(s, Value)
    when
        null == Value
        orelse
        undefined == Value ->
    {ok,0};
cloak_validate(s, Value)
    when
        is_integer(Value)
        andalso
        Value > 0 ->
    {ok,Value};
cloak_validate(t, Value)
    when
        null == Value
        orelse
        undefined == Value ->
    {ok,undefined};
cloak_validate(t, Value) when is_binary(Value) ->
    case
        lists:member(Value,
                     [<<"READY">>,
                      <<"RESUMED">>,
                      <<"CHANNEL_CREATE">>,
                      <<"CHANNEL_UPDATE">>,
                      <<"CHANNEL_DELETE">>,
                      <<"GUILD_CREATE">>,
                      <<"GUILD_UPDATE">>,
                      <<"GUILD_DELETE">>,
                      <<"GUILD_BAN_ADD">>,
                      <<"GUILD_BAN_REMOVE">>,
                      <<"GUILD_EMOJIS_UPDATE">>,
                      <<"GUILD_INTEGRATIONS_UPDATE">>,
                      <<"GUILD_MEMBER_ADD">>,
                      <<"GUILD_MEMBER_REMOVE">>,
                      <<"GUILD_MEMBER_UPDATE">>,
                      <<"GUILD_MEMBERS_CHUNK">>,
                      <<"GUILD_ROLE_CREATE">>,
                      <<"GUILD_ROLE_UPDATE">>,
                      <<"GUILD_ROLE_DELETE">>,
                      <<"MESSAGE_CREATE">>,
                      <<"MESSAGE_UPDATE">>,
                      <<"MESSAGE_DELETE">>,
                      <<"MESSAGE_DELETE_BULK">>,
                      <<"MESSAGE_REACTION_ADD">>,
                      <<"MESSAGE_REACTION_REMOVE">>,
                      <<"MESSAGE_REACTION_REMOVE_ALL">>,
                      <<"PRESENCE_UPDATE">>,
                      <<"TYPING_START">>,
                      <<"USER_UPDATE">>,
                      <<"VOICE_STATE_UPDATE">>,
                      <<"VOICE_SERVER_UPDATE">>])
    of
        true ->
            {ok,Value};
        false ->
            {error,invalid}
    end;
cloak_validate(_, _) ->
    {error,invalid}.
cloak_validate_struct(#marvin_pdu2{op = 0,d = Data,t = Event} = Struct)
    when Event /= undefined ->
    Mod = dispatch_event_to_mod(Event),
    {ok,Struct#marvin_pdu2{prot_mod = Mod,d = Mod:new(Data)}};
cloak_validate_struct(#marvin_pdu2{op = Op,d = Data,t = undefined} =
                          Struct) ->
    Mod = op_to_mod(Op),
    {ok,Struct#marvin_pdu2{prot_mod = Mod,d = Mod:new(Data)}};
cloak_validate_struct(_Struct) ->
    {error,invalid}.
dispatch_event_to_mod(<<"READY">>) ->
    marvin_pdu2_dispatch_ready;
dispatch_event_to_mod(<<"RESUMED">>) ->
    marvin_pdu2_dispatch_resumed;
dispatch_event_to_mod(<<"CHANNEL_CREATE">>) ->
    marvin_pdu2_dispatch_channel_create;
dispatch_event_to_mod(<<"CHANNEL_UPDATE">>) ->
    marvin_pdu2_dispatch_channel_update;
dispatch_event_to_mod(<<"CHANNEL_DELETE">>) ->
    marvin_pdu2_dispatch_channel_delete;
dispatch_event_to_mod(<<"GUILD_CREATE">>) ->
    marvin_pdu2_dispatch_guild_create;
dispatch_event_to_mod(<<"GUILD_UPDATE">>) ->
    marvin_pdu2_dispatch_guild_update;
dispatch_event_to_mod(<<"GUILD_DELETE">>) ->
    marvin_pdu2_dispatch_guild_delete;
dispatch_event_to_mod(<<"GUILD_BAN_ADD">>) ->
    marvin_pdu2_dispatch_guild_ban_add;
dispatch_event_to_mod(<<"GUILD_BAN_REMOVE">>) ->
    marvin_pdu2_dispatch_guild_ban_remove;
dispatch_event_to_mod(<<"GUILD_EMOJIS_UPDATE">>) ->
    marvin_pdu2_dispatch_guild_emojis_update;
dispatch_event_to_mod(<<"GUILD_INTEGRATIONS_UPDATE">>) ->
    marvin_pdu2_dispatch_guild_integrations_update;
dispatch_event_to_mod(<<"GUILD_MEMBER_ADD">>) ->
    marvin_pdu2_dispatch_guild_member_add;
dispatch_event_to_mod(<<"GUILD_MEMBER_REMOVE">>) ->
    marvin_pdu2_dispatch_guild_member_remove;
dispatch_event_to_mod(<<"GUILD_MEMBER_UPDATE">>) ->
    marvin_pdu2_dispatch_guild_member_update;
dispatch_event_to_mod(<<"GUILD_MEMBERS_CHUNK">>) ->
    marvin_pdu2_dispatch_guild_members_chunk;
dispatch_event_to_mod(<<"GUILD_ROLE_CREATE">>) ->
    marvin_pdu2_dispatch_guild_role_create;
dispatch_event_to_mod(<<"GUILD_ROLE_UPDATE">>) ->
    marvin_pdu2_dispatch_guild_role_update;
dispatch_event_to_mod(<<"GUILD_ROLE_DELETE">>) ->
    marvin_pdu2_dispatch_guild_role_delete;
dispatch_event_to_mod(<<"MESSAGE_CREATE">>) ->
    marvin_pdu2_dispatch_message_create;
dispatch_event_to_mod(<<"MESSAGE_UPDATE">>) ->
    marvin_pdu2_dispatch_message_update;
dispatch_event_to_mod(<<"MESSAGE_DELETE">>) ->
    marvin_pdu2_dispatch_message_delete;
dispatch_event_to_mod(<<"MESSAGE_DELETE_BULK">>) ->
    marvin_pdu2_dispatch_message_delete_bulk;
dispatch_event_to_mod(<<"MESSAGE_REACTION_ADD">>) ->
    marvin_pdu2_dispatch_message_reaction_add;
dispatch_event_to_mod(<<"MESSAGE_REACTION_REMOVE">>) ->
    marvin_pdu2_dispatch_message_reaction_remove;
dispatch_event_to_mod(<<"MESSAGE_REACTION_REMOVE_ALL">>) ->
    marvin_pdu2_dispatch_message_reaction_remove_all;
dispatch_event_to_mod(<<"PRESENCE_UPDATE">>) ->
    marvin_pdu2_dispatch_presence_update;
dispatch_event_to_mod(<<"TYPING_START">>) ->
    marvin_pdu2_dispatch_typing_start;
dispatch_event_to_mod(<<"USER_UPDATE">>) ->
    marvin_pdu2_dispatch_user_update;
dispatch_event_to_mod(<<"VOICE_STATE_UPDATE">>) ->
    marvin_pdu2_dispatch_voice_state_update;
dispatch_event_to_mod(<<"VOICE_SERVER_UPDATE">>) ->
    marvin_pdu2_dispatch_voice_server_update.
op_to_mod(1) ->
    marvin_pdu2_heartbeat;
op_to_mod(2) ->
    marvin_pdu2_identify;
op_to_mod(3) ->
    marvin_pdu2_status_update;
op_to_mod(4) ->
    marvin_pdu2_voice_state_update;
op_to_mod(5) ->
    marvin_pdu2_voice_server_ping;
op_to_mod(6) ->
    marvin_pdu2_resume;
op_to_mod(7) ->
    marvin_pdu2_reconnect;
op_to_mod(8) ->
    marvin_pdu2_request_guild_members;
op_to_mod(9) ->
    marvin_pdu2_invalid_session;
op_to_mod(10) ->
    marvin_pdu2_hello;
op_to_mod(11) ->
    marvin_pdu2_heartbeat_ack.

