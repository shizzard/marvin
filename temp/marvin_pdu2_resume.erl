-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_resume.erl",
      1).
-module(marvin_pdu2_resume).
-export([export/1]).
-record(marvin_pdu2_resume,{token :: marvin_pdu2:token(),
                            session_id :: marvin_pdu2:session_id(),
                            seq :: marvin_pdu2:sequence()}).
-type t() :: #marvin_pdu2_resume{}.
-export_type([t/0]).
-export([token/2,
         session_id/2,
         seq/2,
         token/1,
         session_id/1,
         seq/1,
         update/2,
         new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_resume{},
                                                        [{seq,
                                                          <<115,101,113>>},
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
                         marvin_pdu2_resume:Var_key_0(Var_record_0,
                                                      maps:get(Var_key_0,
                                                               Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_resume:Var_key_0(Var_record_0,
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
                         marvin_pdu2_resume:Var_key_0(Var_record_0,
                                                      maps:get(Var_key_0,
                                                               Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_resume:Var_key_0(Var_record_0,
                                                      maps:get(Var_binkey_0,
                                                               Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_resume{} = Var_record_0, #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{seq,<<115,101,113>>},
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
seq(#marvin_pdu2_resume{seq = Var_seq_0}) ->
    Var_seq_0;
seq(_) ->
    error(badarg).
session_id(#marvin_pdu2_resume{session_id = Var_session_id_0}) ->
    Var_session_id_0;
session_id(_) ->
    error(badarg).
token(#marvin_pdu2_resume{token = Var_token_0}) ->
    Var_token_0;
token(_) ->
    error(badarg).
seq(#marvin_pdu2_resume{} = Var_record_0, Var_value_0) ->
    case cloak_validate(seq, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(seq,
                          Var_record_0#marvin_pdu2_resume{seq =
                                                              Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [seq,Var_reason_0]),
            error(badarg)
    end;
seq(_, _) ->
    error(badarg).
session_id(#marvin_pdu2_resume{} = Var_record_0, Var_value_0) ->
    case cloak_validate(session_id, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(session_id,
                          Var_record_0#marvin_pdu2_resume{session_id =
                                                              Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [session_id,Var_reason_0]),
            error(badarg)
    end;
session_id(_, _) ->
    error(badarg).
token(#marvin_pdu2_resume{} = Var_record_0, Var_value_0) ->
    case cloak_validate(token, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(token,
                          Var_record_0#marvin_pdu2_resume{token =
                                                              Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [token,Var_reason_0]),
            error(badarg)
    end;
token(_, _) ->
    error(badarg).
cloak_validate_struct(Var_value_0) ->
    {ok,Var_value_0}.
cloak_updated(_, Var_record_0) ->
    Var_record_0.

cloak_validate(token, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(session_id, Value)
    when
        is_binary(Value)
        andalso
        Value /= <<>> ->
    {ok,Value};
cloak_validate(seq, Value)
    when
        is_integer(Value)
        andalso
        Value > 0 ->
    {ok,Value};
cloak_validate(_, _) ->
    {error,invalid}.
export(#marvin_pdu2_resume{token = Token,
                           session_id = SessionId,
                           seq = Sequence}) ->
    #{<<"token">> => Token,
      <<"session_id">> => SessionId,
      <<"seq">> => Sequence}.

