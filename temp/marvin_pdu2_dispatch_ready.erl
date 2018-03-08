-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_dispatch_ready.erl",
      1).
-module(marvin_pdu2_dispatch_ready).
-export([export/1]).
-record(marvin_pdu2_dispatch_ready,{guilds ::
                                        [marvin_pdu2_object_guild_unavailable:t()],
                                    presences ::
                                        [marvin_pdu2_object_user_presence:t()],
                                    private_channels ::
                                        [marvin_pdu_object_channel_dm:t()],
                                    relationships ::
                                        [marvin_pdu_object_user_relationship:t()],
                                    session_id ::
                                        marvin_pdu2:session_id(),
                                    shard :: marvin_pdu2_shard:t(),
                                    user :: marvin_pdu_object_user:t(),
                                    user_settings ::
                                        marvin_pdu_object_user_settings:t(),
                                    v :: marvin_pdu2:protocol_version(),
                                    '_trace' :: marvin_pdu2:trace()}).
-type t() :: #marvin_pdu2_dispatch_ready{}.
-export_type([t/0]).
-export([guilds/2,
         presences/2,
         private_channels/2,
         relationships/2,
         session_id/2,
         shard/2,
         user/2,
         user_settings/2,
         v/2,
         '_trace'/2,
         guilds/1,
         presences/1,
         private_channels/1,
         relationships/1,
         session_id/1,
         shard/1,
         user/1,
         user_settings/1,
         v/1,
         '_trace'/1,
         update/2,
         new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_dispatch_ready{},
                                                        [{'_trace',
                                                          <<95,
                                                            116,
                                                            114,
                                                            97,
                                                            99,
                                                            101>>},
                                                         {v,<<118>>},
                                                         {user_settings,
                                                          <<117,
                                                            115,
                                                            101,
                                                            114,
                                                            95,
                                                            115,
                                                            101,
                                                            116,
                                                            116,
                                                            105,
                                                            110,
                                                            103,
                                                            115>>},
                                                         {user,
                                                          <<117,
                                                            115,
                                                            101,
                                                            114>>},
                                                         {shard,
                                                          <<115,
                                                            104,
                                                            97,
                                                            114,
                                                            100>>},
                                                         {session_id,
                                                          <<115,
                                                            101,
                                                            115,
                                                            115,
                                                            105,
                                                            111,
                                                            110,
                                                            95,
                                                            105,
                                                            100>>},
                                                         {relationships,
                                                          <<114,
                                                            101,
                                                            108,
                                                            97,
                                                            116,
                                                            105,
                                                            111,
                                                            110,
                                                            115,
                                                            104,
                                                            105,
                                                            112,
                                                            115>>},
                                                         {private_channels,
                                                          <<112,
                                                            114,
                                                            105,
                                                            118,
                                                            97,
                                                            116,
                                                            101,
                                                            95,
                                                            99,
                                                            104,
                                                            97,
                                                            110,
                                                            110,
                                                            101,
                                                            108,
                                                            115>>},
                                                         {presences,
                                                          <<112,
                                                            114,
                                                            101,
                                                            115,
                                                            101,
                                                            110,
                                                            99,
                                                            101,
                                                            115>>},
                                                         {guilds,
                                                          <<103,
                                                            117,
                                                            105,
                                                            108,
                                                            100,
                                                            115>>}]),
                                           []))
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
                         marvin_pdu2_dispatch_ready:Var_key_0(Var_record_0,
                                                              maps:get(Var_key_0,
                                                                       Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_dispatch_ready:Var_key_0(Var_record_0,
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
                         marvin_pdu2_dispatch_ready:Var_key_0(Var_record_0,
                                                              maps:get(Var_key_0,
                                                                       Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_dispatch_ready:Var_key_0(Var_record_0,
                                                              maps:get(Var_binkey_0,
                                                                       Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_dispatch_ready{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{'_trace',
                                             <<95,116,114,97,99,101>>},
                                            {v,<<118>>},
                                            {user_settings,
                                             <<117,
                                               115,
                                               101,
                                               114,
                                               95,
                                               115,
                                               101,
                                               116,
                                               116,
                                               105,
                                               110,
                                               103,
                                               115>>},
                                            {user,<<117,115,101,114>>},
                                            {shard,
                                             <<115,104,97,114,100>>},
                                            {session_id,
                                             <<115,
                                               101,
                                               115,
                                               115,
                                               105,
                                               111,
                                               110,
                                               95,
                                               105,
                                               100>>},
                                            {relationships,
                                             <<114,
                                               101,
                                               108,
                                               97,
                                               116,
                                               105,
                                               111,
                                               110,
                                               115,
                                               104,
                                               105,
                                               112,
                                               115>>},
                                            {private_channels,
                                             <<112,
                                               114,
                                               105,
                                               118,
                                               97,
                                               116,
                                               101,
                                               95,
                                               99,
                                               104,
                                               97,
                                               110,
                                               110,
                                               101,
                                               108,
                                               115>>},
                                            {presences,
                                             <<112,
                                               114,
                                               101,
                                               115,
                                               101,
                                               110,
                                               99,
                                               101,
                                               115>>},
                                            {guilds,
                                             <<103,117,105,108,100,115>>}]))
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
'_trace'(#marvin_pdu2_dispatch_ready{'_trace' = Var__trace_0}) ->
    Var__trace_0;
'_trace'(_) ->
    error(badarg).
v(#marvin_pdu2_dispatch_ready{v = Var_v_0}) ->
    Var_v_0;
v(_) ->
    error(badarg).
user_settings(#marvin_pdu2_dispatch_ready{user_settings =
                                              Var_user_settings_0}) ->
    Var_user_settings_0;
user_settings(_) ->
    error(badarg).
user(#marvin_pdu2_dispatch_ready{user = Var_user_0}) ->
    Var_user_0;
user(_) ->
    error(badarg).
shard(#marvin_pdu2_dispatch_ready{shard = Var_shard_0}) ->
    Var_shard_0;
shard(_) ->
    error(badarg).
session_id(#marvin_pdu2_dispatch_ready{session_id = Var_session_id_0}) ->
    Var_session_id_0;
session_id(_) ->
    error(badarg).
relationships(#marvin_pdu2_dispatch_ready{relationships =
                                              Var_relationships_0}) ->
    Var_relationships_0;
relationships(_) ->
    error(badarg).
private_channels(#marvin_pdu2_dispatch_ready{private_channels =
                                                 Var_private_channels_0}) ->
    Var_private_channels_0;
private_channels(_) ->
    error(badarg).
presences(#marvin_pdu2_dispatch_ready{presences = Var_presences_0}) ->
    Var_presences_0;
presences(_) ->
    error(badarg).
guilds(#marvin_pdu2_dispatch_ready{guilds = Var_guilds_0}) ->
    Var_guilds_0;
guilds(_) ->
    error(badarg).
'_trace'(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate('_trace', Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated('_trace',
                          Var_record_0#marvin_pdu2_dispatch_ready{'_trace' =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   ['_trace',Var_reason_0]),
            error(badarg)
    end;
'_trace'(_, _) ->
    error(badarg).
v(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(v, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(v,
                          Var_record_0#marvin_pdu2_dispatch_ready{v = Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [v,Var_reason_0]),
            error(badarg)
    end;
v(_, _) ->
    error(badarg).
user_settings(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(user_settings, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(user_settings,
                          Var_record_0#marvin_pdu2_dispatch_ready{user_settings =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [user_settings,Var_reason_0]),
            error(badarg)
    end;
user_settings(_, _) ->
    error(badarg).
user(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(user, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(user,
                          Var_record_0#marvin_pdu2_dispatch_ready{user =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [user,Var_reason_0]),
            error(badarg)
    end;
user(_, _) ->
    error(badarg).
shard(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(shard, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(shard,
                          Var_record_0#marvin_pdu2_dispatch_ready{shard =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [shard,Var_reason_0]),
            error(badarg)
    end;
shard(_, _) ->
    error(badarg).
session_id(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(session_id, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(session_id,
                          Var_record_0#marvin_pdu2_dispatch_ready{session_id =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [session_id,Var_reason_0]),
            error(badarg)
    end;
session_id(_, _) ->
    error(badarg).
relationships(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(relationships, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(relationships,
                          Var_record_0#marvin_pdu2_dispatch_ready{relationships =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [relationships,Var_reason_0]),
            error(badarg)
    end;
relationships(_, _) ->
    error(badarg).
private_channels(#marvin_pdu2_dispatch_ready{} = Var_record_0,
                 Var_value_0) ->
    case cloak_validate(private_channels, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(private_channels,
                          Var_record_0#marvin_pdu2_dispatch_ready{private_channels =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [private_channels,Var_reason_0]),
            error(badarg)
    end;
private_channels(_, _) ->
    error(badarg).
presences(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(presences, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(presences,
                          Var_record_0#marvin_pdu2_dispatch_ready{presences =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [presences,Var_reason_0]),
            error(badarg)
    end;
presences(_, _) ->
    error(badarg).
guilds(#marvin_pdu2_dispatch_ready{} = Var_record_0, Var_value_0) ->
    case cloak_validate(guilds, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(guilds,
                          Var_record_0#marvin_pdu2_dispatch_ready{guilds =
                                                                      Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [guilds,Var_reason_0]),
            error(badarg)
    end;
guilds(_, _) ->
    error(badarg).
cloak_validate_struct(Var_value_0) ->
    {ok,Var_value_0}.
cloak_updated(_, Var_record_0) ->
    Var_record_0.

cloak_validate(guilds, Value) when is_list(Value) ->
    {ok,
     [ 
      marvin_pdu2_object_guild_unavailable:new(Item) ||
          Item <- Value
     ]};
cloak_validate(presences, Value) when is_list(Value) ->
    {ok,
     [ 
      marvin_pdu2_object_user_presence:new(Item) ||
          Item <- Value
     ]};
cloak_validate(private_channels, Value) when is_list(Value) ->
    {ok,
     [ 
      marvin_pdu_object_channel_dm:new(Item) ||
          Item <- Value
     ]};
cloak_validate(relationships, Value) when is_list(Value) ->
    {ok,
     [ 
      marvin_pdu_object_user_relationship:new(Item) ||
          Item <- Value
     ]};
cloak_validate(session_id, Value) when is_binary(Value) ->
    {ok,Value};
cloak_validate(shard, [Shard,TotalShards]) ->
    {ok,
     marvin_pdu2_shard:new(#{shard => Shard,total_shards => TotalShards})};
cloak_validate(user, Value) ->
    {ok,marvin_pdu2_object_user:new(Value)};
cloak_validate(user_settings, Value) ->
    {ok,marvin_pdu_object_user_settings:new(Value)};
cloak_validate(v, Value)
    when
        is_integer(Value)
        andalso
        Value > 0 ->
    {ok,Value};
cloak_validate('_trace', Value) ->
    case lists:all(fun is_binary/1, Value) of
        true ->
            {ok,Value};
        false ->
            {error,invalid}
    end;
cloak_validate(_, _) ->
    {error,invalid}.
export(#marvin_pdu2_dispatch_ready{guilds = Guilds,
                                   presences = Presences,
                                   private_channels = PrivateChannels,
                                   relationships = Relationships,
                                   session_id = SessionId,
                                   shard = Shard,
                                   user = User,
                                   user_settings = UserSettings,
                                   v = Version,
                                   '_trace' = Trace}) ->
    #{<<"guilds">> =>
          [ 
           marvin_pdu2_object_guild_unavailable:export(Item) ||
               Item <- Guilds
          ],
      <<"presences">> =>
          [ 
           marvin_pdu2_object_user_presence:export(Item) ||
               Item <- Presences
          ],
      <<"private_channels">> =>
          [ 
           marvin_pdu_object_channel_dm:export(Item) ||
               Item <- PrivateChannels
          ],
      <<"relationships">> =>
          [ 
           marvin_pdu_object_user_relationship:export(Item) ||
               Item <- Relationships
          ],
      <<"session_id">> => SessionId,
      <<"shard">> =>
          [marvin_pdu2_shard:shard(Shard),
           marvin_pdu2_shard:total_shards(Shard)],
      <<"user">> => marvin_pdu_object_user:export(User),
      <<"user_settings">> =>
          marvin_pdu_object_user_settings:export(UserSettings),
      <<"v">> => Version,
      <<"_trace">> => Trace}.

