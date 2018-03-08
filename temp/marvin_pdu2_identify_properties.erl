-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_identify_properties.erl",
      1).
-module(marvin_pdu2_identify_properties).
-export([export/1]).
-record(marvin_pdu2_identify_properties,{'$os' ::
                                             marvin_pdu2:properties_os(),
                                         '$browser' ::
                                             marvin_pdu2:properties_browser(),
                                         '$device' ::
                                             marvin_pdu2:properties_device(),
                                         '$referrer' ::
                                             marvin_pdu2:properties_referrer(),
                                         '$referring_domain' ::
                                             marvin_pdu2:properties_referring_domain()}).
-type t() :: #marvin_pdu2_identify_properties{}.
-export_type([t/0]).
-export(['$os'/2,
         '$browser'/2,
         '$device'/2,
         '$referrer'/2,
         '$referring_domain'/2,
         '$os'/1,
         '$browser'/1,
         '$device'/1,
         '$referrer'/1,
         '$referring_domain'/1,
         update/2,
         new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_identify_properties{},
                                                        [{'$referring_domain',
                                                          <<36,
                                                            114,
                                                            101,
                                                            102,
                                                            101,
                                                            114,
                                                            114,
                                                            105,
                                                            110,
                                                            103,
                                                            95,
                                                            100,
                                                            111,
                                                            109,
                                                            97,
                                                            105,
                                                            110>>},
                                                         {'$referrer',
                                                          <<36,
                                                            114,
                                                            101,
                                                            102,
                                                            101,
                                                            114,
                                                            114,
                                                            101,
                                                            114>>},
                                                         {'$device',
                                                          <<36,
                                                            100,
                                                            101,
                                                            118,
                                                            105,
                                                            99,
                                                            101>>},
                                                         {'$browser',
                                                          <<36,
                                                            98,
                                                            114,
                                                            111,
                                                            119,
                                                            115,
                                                            101,
                                                            114>>},
                                                         {'$os',
                                                          <<36,111,115>>}]),
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
                         marvin_pdu2_identify_properties:Var_key_0(Var_record_0,
                                                                   maps:get(Var_key_0,
                                                                            Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_identify_properties:Var_key_0(Var_record_0,
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
                         marvin_pdu2_identify_properties:Var_key_0(Var_record_0,
                                                                   maps:get(Var_key_0,
                                                                            Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_identify_properties:Var_key_0(Var_record_0,
                                                                   maps:get(Var_binkey_0,
                                                                            Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_identify_properties{} = Var_record_0,
       #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{'$referring_domain',
                                             <<36,
                                               114,
                                               101,
                                               102,
                                               101,
                                               114,
                                               114,
                                               105,
                                               110,
                                               103,
                                               95,
                                               100,
                                               111,
                                               109,
                                               97,
                                               105,
                                               110>>},
                                            {'$referrer',
                                             <<36,
                                               114,
                                               101,
                                               102,
                                               101,
                                               114,
                                               114,
                                               101,
                                               114>>},
                                            {'$device',
                                             <<36,100,101,118,105,99,101>>},
                                            {'$browser',
                                             <<36,
                                               98,
                                               114,
                                               111,
                                               119,
                                               115,
                                               101,
                                               114>>},
                                            {'$os',<<36,111,115>>}]))
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
'$referring_domain'(#marvin_pdu2_identify_properties{'$referring_domain' =
                                                         Var_$referring_domain_0}) ->
    Var_$referring_domain_0;
'$referring_domain'(_) ->
    error(badarg).
'$referrer'(#marvin_pdu2_identify_properties{'$referrer' =
                                                 Var_$referrer_0}) ->
    Var_$referrer_0;
'$referrer'(_) ->
    error(badarg).
'$device'(#marvin_pdu2_identify_properties{'$device' = Var_$device_0}) ->
    Var_$device_0;
'$device'(_) ->
    error(badarg).
'$browser'(#marvin_pdu2_identify_properties{'$browser' = Var_$browser_0}) ->
    Var_$browser_0;
'$browser'(_) ->
    error(badarg).
'$os'(#marvin_pdu2_identify_properties{'$os' = Var_$os_0}) ->
    Var_$os_0;
'$os'(_) ->
    error(badarg).
'$referring_domain'(#marvin_pdu2_identify_properties{} = Var_record_0,
                    Var_value_0) ->
    case cloak_validate('$referring_domain', Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated('$referring_domain',
                          Var_record_0#marvin_pdu2_identify_properties{'$referring_domain' =
                                                                           Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   ['$referring_domain',Var_reason_0]),
            error(badarg)
    end;
'$referring_domain'(_, _) ->
    error(badarg).
'$referrer'(#marvin_pdu2_identify_properties{} = Var_record_0,
            Var_value_0) ->
    case cloak_validate('$referrer', Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated('$referrer',
                          Var_record_0#marvin_pdu2_identify_properties{'$referrer' =
                                                                           Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   ['$referrer',Var_reason_0]),
            error(badarg)
    end;
'$referrer'(_, _) ->
    error(badarg).
'$device'(#marvin_pdu2_identify_properties{} = Var_record_0,
          Var_value_0) ->
    case cloak_validate('$device', Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated('$device',
                          Var_record_0#marvin_pdu2_identify_properties{'$device' =
                                                                           Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   ['$device',Var_reason_0]),
            error(badarg)
    end;
'$device'(_, _) ->
    error(badarg).
'$browser'(#marvin_pdu2_identify_properties{} = Var_record_0,
           Var_value_0) ->
    case cloak_validate('$browser', Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated('$browser',
                          Var_record_0#marvin_pdu2_identify_properties{'$browser' =
                                                                           Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   ['$browser',Var_reason_0]),
            error(badarg)
    end;
'$browser'(_, _) ->
    error(badarg).
'$os'(#marvin_pdu2_identify_properties{} = Var_record_0, Var_value_0) ->
    case cloak_validate('$os', Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated('$os',
                          Var_record_0#marvin_pdu2_identify_properties{'$os' =
                                                                           Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   ['$os',Var_reason_0]),
            error(badarg)
    end;
'$os'(_, _) ->
    error(badarg).
cloak_validate_struct(Var_value_0) ->
    {ok,Var_value_0}.
cloak_updated(_, Var_record_0) ->
    Var_record_0.

cloak_validate('$os', Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate('$browser', Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate('$device', Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate('$referrer', Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate('$referring_domain', Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(_, _) ->
    {error,invalid}.
export(#marvin_pdu2_identify_properties{'$os' = OS,
                                        '$browser' = Browser,
                                        '$device' = Device,
                                        '$referrer' = Referrer,
                                        '$referring_domain' =
                                            ReferringDomain}) ->
    #{<<"$os">> => OS,
      <<"$browser">> => Browser,
      <<"$device">> => Device,
      <<"$referrer">> => Referrer,
      <<"$referring_domain">> => ReferringDomain}.

