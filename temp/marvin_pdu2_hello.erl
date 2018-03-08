-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_hello.erl",
      1).
-module(marvin_pdu2_hello).
-export([export/1]).
-record(marvin_pdu2_hello,{heartbeat_interval ::
                               marvin_pdu2:heartbeat_interval(),
                           '_trace' :: marvin_pdu2:trace()}).
-type t() :: #marvin_pdu2_hello{}.
-export_type([t/0]).
-export([heartbeat_interval/2,
         '_trace'/2,
         heartbeat_interval/1,
         '_trace'/1,
         update/2,
         new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_hello{},
                                                        [{'_trace',
                                                          <<95,
                                                            116,
                                                            114,
                                                            97,
                                                            99,
                                                            101>>},
                                                         {heartbeat_interval,
                                                          <<104,
                                                            101,
                                                            97,
                                                            114,
                                                            116,
                                                            98,
                                                            101,
                                                            97,
                                                            116,
                                                            95,
                                                            105,
                                                            110,
                                                            116,
                                                            101,
                                                            114,
                                                            118,
                                                            97,
                                                            108>>}]),
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
                         marvin_pdu2_hello:Var_key_0(Var_record_0,
                                                     maps:get(Var_key_0,
                                                              Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_hello:Var_key_0(Var_record_0,
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
                         marvin_pdu2_hello:Var_key_0(Var_record_0,
                                                     maps:get(Var_key_0,
                                                              Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_hello:Var_key_0(Var_record_0,
                                                     maps:get(Var_binkey_0,
                                                              Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_hello{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{'_trace',
                                             <<95,116,114,97,99,101>>},
                                            {heartbeat_interval,
                                             <<104,
                                               101,
                                               97,
                                               114,
                                               116,
                                               98,
                                               101,
                                               97,
                                               116,
                                               95,
                                               105,
                                               110,
                                               116,
                                               101,
                                               114,
                                               118,
                                               97,
                                               108>>}]))
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
'_trace'(#marvin_pdu2_hello{'_trace' = Var__trace_0}) ->
    Var__trace_0;
'_trace'(_) ->
    error(badarg).
heartbeat_interval(#marvin_pdu2_hello{heartbeat_interval =
                                          Var_heartbeat_interval_0}) ->
    Var_heartbeat_interval_0;
heartbeat_interval(_) ->
    error(badarg).
'_trace'(#marvin_pdu2_hello{} = Var_record_0, Var_value_0) ->
    case cloak_validate('_trace', Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated('_trace',
                          Var_record_0#marvin_pdu2_hello{'_trace' =
                                                             Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   ['_trace',Var_reason_0]),
            error(badarg)
    end;
'_trace'(_, _) ->
    error(badarg).
heartbeat_interval(#marvin_pdu2_hello{} = Var_record_0, Var_value_0) ->
    case cloak_validate(heartbeat_interval, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(heartbeat_interval,
                          Var_record_0#marvin_pdu2_hello{heartbeat_interval =
                                                             Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [heartbeat_interval,Var_reason_0]),
            error(badarg)
    end;
heartbeat_interval(_, _) ->
    error(badarg).
cloak_validate_struct(Var_value_0) ->
    {ok,Var_value_0}.
cloak_updated(_, Var_record_0) ->
    Var_record_0.

cloak_validate(heartbeat_interval, Value) when Value > 0 ->
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
export(#marvin_pdu2_hello{heartbeat_interval = HeartbeatInterval,
                          '_trace' = Trace}) ->
    #{<<"heartbeat_interval">> => HeartbeatInterval,
      <<"_trace">> => Trace}.

