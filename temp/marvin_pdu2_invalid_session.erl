-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_invalid_session.erl",
      1).
-module(marvin_pdu2_invalid_session).
-export([export/1]).
-record(marvin_pdu2_invalid_session,{ack = true :: true}).
-type t() :: #marvin_pdu2_invalid_session{}.
-export_type([t/0]).
-export([ack/2,ack/1,update/2,new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_invalid_session{},
                                                        []),
                                           [{ack,<<97,99,107>>}]))
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
                         marvin_pdu2_invalid_session:Var_key_0(Var_record_0,
                                                               maps:get(Var_key_0,
                                                                        Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_invalid_session:Var_key_0(Var_record_0,
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
                         marvin_pdu2_invalid_session:Var_key_0(Var_record_0,
                                                               maps:get(Var_key_0,
                                                                        Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_invalid_session:Var_key_0(Var_record_0,
                                                               maps:get(Var_binkey_0,
                                                                        Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_invalid_session{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{ack,<<97,99,107>>}]))
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
ack(#marvin_pdu2_invalid_session{ack = Var_ack_0}) ->
    Var_ack_0;
ack(_) ->
    error(badarg).
ack(#marvin_pdu2_invalid_session{} = Var_record_0, Var_value_0) ->
    case cloak_validate(ack, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(ack,
                          Var_record_0#marvin_pdu2_invalid_session{ack =
                                                                       Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [ack,Var_reason_0]),
            error(badarg)
    end;
ack(_, _) ->
    error(badarg).
cloak_validate_struct(Var_value_0) ->
    {ok,Var_value_0}.
cloak_validate(_, Var_value_0) ->
    {ok,Var_value_0}.
cloak_updated(_, Var_record_0) ->
    Var_record_0.

export(#marvin_pdu2_invalid_session{}) ->
    #{}.

