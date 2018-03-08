-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_identify.erl",
      1).
-module(marvin_pdu2_identify).
-export([export/1]).
-record(marvin_pdu2_identify,{token :: marvin_pdu2:token(),
                              compress :: marvin_pdu2:compress(),
                              large_threshold ::
                                  marvin_pdu2:large_threshold(),
                              properties ::
                                  marvin_pdu2_identify_properties:t(),
                              shard :: marvin_pdu2:shard_spec(),
                              prot_shard :: marvin_pdu2:shard(),
                              prot_total_shards ::
                                  marvin_pdu2:total_shards()}).
-type t() :: #marvin_pdu2_identify{}.
-export_type([t/0]).
-export([token/2,
         compress/2,
         large_threshold/2,
         properties/2,
         shard/2,
         prot_shard/1,
         prot_total_shards/1,
         token/1,
         compress/1,
         large_threshold/1,
         properties/1,
         shard/1,
         update/2,
         new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_identify{},
                                                        [{shard,
                                                          <<115,
                                                            104,
                                                            97,
                                                            114,
                                                            100>>},
                                                         {properties,
                                                          <<112,
                                                            114,
                                                            111,
                                                            112,
                                                            101,
                                                            114,
                                                            116,
                                                            105,
                                                            101,
                                                            115>>},
                                                         {large_threshold,
                                                          <<108,
                                                            97,
                                                            114,
                                                            103,
                                                            101,
                                                            95,
                                                            116,
                                                            104,
                                                            114,
                                                            101,
                                                            115,
                                                            104,
                                                            111,
                                                            108,
                                                            100>>},
                                                         {compress,
                                                          <<99,
                                                            111,
                                                            109,
                                                            112,
                                                            114,
                                                            101,
                                                            115,
                                                            115>>},
                                                         {token,
                                                          <<116,
                                                            111,
                                                            107,
                                                            101,
                                                            110>>}]),
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
                         marvin_pdu2_identify:Var_key_0(Var_record_0,
                                                        maps:get(Var_key_0,
                                                                 Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_identify:Var_key_0(Var_record_0,
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
                         marvin_pdu2_identify:Var_key_0(Var_record_0,
                                                        maps:get(Var_key_0,
                                                                 Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_identify:Var_key_0(Var_record_0,
                                                        maps:get(Var_binkey_0,
                                                                 Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_identify{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{shard,
                                             <<115,104,97,114,100>>},
                                            {properties,
                                             <<112,
                                               114,
                                               111,
                                               112,
                                               101,
                                               114,
                                               116,
                                               105,
                                               101,
                                               115>>},
                                            {large_threshold,
                                             <<108,
                                               97,
                                               114,
                                               103,
                                               101,
                                               95,
                                               116,
                                               104,
                                               114,
                                               101,
                                               115,
                                               104,
                                               111,
                                               108,
                                               100>>},
                                            {compress,
                                             <<99,
                                               111,
                                               109,
                                               112,
                                               114,
                                               101,
                                               115,
                                               115>>},
                                            {token,
                                             <<116,111,107,101,110>>}]))
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
shard(#marvin_pdu2_identify{shard = Var_shard_0}) ->
    Var_shard_0;
shard(_) ->
    error(badarg).
properties(#marvin_pdu2_identify{properties = Var_properties_0}) ->
    Var_properties_0;
properties(_) ->
    error(badarg).
large_threshold(#marvin_pdu2_identify{large_threshold =
                                          Var_large_threshold_0}) ->
    Var_large_threshold_0;
large_threshold(_) ->
    error(badarg).
compress(#marvin_pdu2_identify{compress = Var_compress_0}) ->
    Var_compress_0;
compress(_) ->
    error(badarg).
token(#marvin_pdu2_identify{token = Var_token_0}) ->
    Var_token_0;
token(_) ->
    error(badarg).
prot_total_shards(#marvin_pdu2_identify{prot_total_shards =
                                            Var_prot_total_shards_0}) ->
    Var_prot_total_shards_0;
prot_total_shards(_) ->
    error(badarg).
prot_shard(#marvin_pdu2_identify{prot_shard = Var_prot_shard_0}) ->
    Var_prot_shard_0;
prot_shard(_) ->
    error(badarg).
shard(#marvin_pdu2_identify{} = Var_record_0, Var_value_0) ->
    case cloak_validate(shard, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(shard,
                          Var_record_0#marvin_pdu2_identify{shard =
                                                                Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [shard,Var_reason_0]),
            error(badarg)
    end;
shard(_, _) ->
    error(badarg).
properties(#marvin_pdu2_identify{} = Var_record_0, Var_value_0) ->
    case cloak_validate(properties, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(properties,
                          Var_record_0#marvin_pdu2_identify{properties =
                                                                Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [properties,Var_reason_0]),
            error(badarg)
    end;
properties(_, _) ->
    error(badarg).
large_threshold(#marvin_pdu2_identify{} = Var_record_0, Var_value_0) ->
    case cloak_validate(large_threshold, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(large_threshold,
                          Var_record_0#marvin_pdu2_identify{large_threshold =
                                                                Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [large_threshold,Var_reason_0]),
            error(badarg)
    end;
large_threshold(_, _) ->
    error(badarg).
compress(#marvin_pdu2_identify{} = Var_record_0, Var_value_0) ->
    case cloak_validate(compress, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(compress,
                          Var_record_0#marvin_pdu2_identify{compress =
                                                                Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [compress,Var_reason_0]),
            error(badarg)
    end;
compress(_, _) ->
    error(badarg).
token(#marvin_pdu2_identify{} = Var_record_0, Var_value_0) ->
    case cloak_validate(token, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(token,
                          Var_record_0#marvin_pdu2_identify{token =
                                                                Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [token,Var_reason_0]),
            error(badarg)
    end;
token(_, _) ->
    error(badarg).
cloak_updated(_, Var_record_0) ->
    Var_record_0.

cloak_validate(token, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(compress, Value) when is_boolean(Value) ->
    {ok,Value};
cloak_validate(large_threshold, Value)
    when
        Value > 0
        andalso
        Value =< 250 ->
    {ok,Value};
cloak_validate(properties, Value) ->
    {ok,marvin_pdu2_identify_properties:new(Value)};
cloak_validate(shard, [Shard,TotalShards] = Value)
    when
        is_integer(Shard)
        andalso
        is_integer(TotalShards)
        andalso
        Shard < TotalShards ->
    {ok,Value};
cloak_validate(_, _) ->
    {error,invalid}.
cloak_validate_struct(#marvin_pdu2_identify{shard = [Shard,TotalShards]} =
                          Struct) ->
    {ok,
     Struct#marvin_pdu2_identify{prot_shard = Shard,
                                 prot_total_shards = TotalShards}};
cloak_validate_struct(_Struct) ->
    {error,invalid}.
export(#marvin_pdu2_identify{token = Token,
                             compress = Compress,
                             large_threshold = LargeThreshold,
                             properties = Properties,
                             shard = ShardSpec}) ->
    #{<<"token">> => Token,
      <<"compress">> => Compress,
      <<"large_threshold">> => LargeThreshold,
      <<"properties">> =>
          marvin_pdu2_identify_properties:export(Properties),
      <<"shard">> => ShardSpec}.

