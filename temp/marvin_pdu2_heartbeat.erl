-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_heartbeat.erl",
      1).
-module(marvin_pdu2_heartbeat).
-export([export/1]).
-record(marvin_pdu2_heartbeat,{plain_value :: marvin_pdu2:sequence()}).
-type t() :: #marvin_pdu2_heartbeat{}.
-export_type([t/0]).
-export([plain_value/2,plain_value/1,update/2,new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_heartbeat{},
                                                        [{plain_value,
                                                          <<112,
                                                            108,
                                                            97,
                                                            105,
                                                            110,
                                                            95,
                                                            118,
                                                            97,
                                                            108,
                                                            117,
                                                            101>>}]),
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
                         marvin_pdu2_heartbeat:Var_key_0(Var_record_0,
                                                         maps:get(Var_key_0,
                                                                  Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_heartbeat:Var_key_0(Var_record_0,
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
                         marvin_pdu2_heartbeat:Var_key_0(Var_record_0,
                                                         maps:get(Var_key_0,
                                                                  Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_heartbeat:Var_key_0(Var_record_0,
                                                         maps:get(Var_binkey_0,
                                                                  Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_heartbeat{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{plain_value,
                                             <<112,
                                               108,
                                               97,
                                               105,
                                               110,
                                               95,
                                               118,
                                               97,
                                               108,
                                               117,
                                               101>>}]))
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
plain_value(#marvin_pdu2_heartbeat{plain_value = Var_plain_value_0}) ->
    Var_plain_value_0;
plain_value(_) ->
    error(badarg).
plain_value(#marvin_pdu2_heartbeat{} = Var_record_0, Var_value_0) ->
    case cloak_validate(plain_value, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(plain_value,
                          Var_record_0#marvin_pdu2_heartbeat{plain_value =
                                                                 Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [plain_value,Var_reason_0]),
            error(badarg)
    end;
plain_value(_, _) ->
    error(badarg).
cloak_validate_struct(Var_value_0) ->
    {ok,Var_value_0}.
cloak_updated(_, Var_record_0) ->
    Var_record_0.

cloak_validate(plain_value, Value)
    when
        is_integer(Value)
        andalso
        Value > 0 ->
    {ok,Value};
cloak_validate(_, _) ->
    {error,invalid}.
export(#marvin_pdu2_heartbeat{plain_value = LastSeq}) ->
    #{<<"plain_value">> => LastSeq}.

