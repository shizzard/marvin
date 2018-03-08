-file("/Users/shizz/code/private/marvin/_build/test/lib/marvin_pdu2/src"
      "/marvin_pdu2_object_guild_unavailable.erl",
      1).
-module(marvin_pdu2_object_guild_unavailable).
-export([export/1]).
-record(marvin_pdu2_object_guild_unavailable,{id ::
                                                  marvin_pdu2:snowflake(),
                                              unavailable ::
                                                  marvin_pdu2:unavailable()}).
-type t() :: #marvin_pdu2_object_guild_unavailable{}.
-export_type([t/0]).
-export([id/2,unavailable/2,id/1,unavailable/1,update/2,new/1]).
new(#{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           required_new(Var_map_0,
                                                        #marvin_pdu2_object_guild_unavailable{},
                                                        [{unavailable,
                                                          <<117,
                                                            110,
                                                            97,
                                                            118,
                                                            97,
                                                            105,
                                                            108,
                                                            97,
                                                            98,
                                                            108,
                                                            101>>},
                                                         {id,
                                                          <<105,100>>}]),
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
                         marvin_pdu2_object_guild_unavailable:Var_key_0(Var_record_0,
                                                                        maps:get(Var_key_0,
                                                                                 Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_object_guild_unavailable:Var_key_0(Var_record_0,
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
                         marvin_pdu2_object_guild_unavailable:Var_key_0(Var_record_0,
                                                                        maps:get(Var_key_0,
                                                                                 Var_map_0)),
                         Var_keys_0);
        {_,true} ->
            required_new(Var_map_0,
                         marvin_pdu2_object_guild_unavailable:Var_key_0(Var_record_0,
                                                                        maps:get(Var_binkey_0,
                                                                                 Var_map_0)),
                         Var_keys_0);
        {_,_} ->
            optional_new(Var_map_0, Var_record_0, Var_keys_0)
    end.
update(#marvin_pdu2_object_guild_unavailable{} = Var_record_0,
       #{} = Var_map_0) ->
    case
        cloak_validate_struct(optional_new(Var_map_0,
                                           Var_record_0,
                                           [{unavailable,
                                             <<117,
                                               110,
                                               97,
                                               118,
                                               97,
                                               105,
                                               108,
                                               97,
                                               98,
                                               108,
                                               101>>},
                                            {id,<<105,100>>}]))
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
unavailable(#marvin_pdu2_object_guild_unavailable{unavailable =
                                                      Var_unavailable_0}) ->
    Var_unavailable_0;
unavailable(_) ->
    error(badarg).
id(#marvin_pdu2_object_guild_unavailable{id = Var_id_0}) ->
    Var_id_0;
id(_) ->
    error(badarg).
unavailable(#marvin_pdu2_object_guild_unavailable{} = Var_record_0,
            Var_value_0) ->
    case cloak_validate(unavailable, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(unavailable,
                          Var_record_0#marvin_pdu2_object_guild_unavailable{unavailable =
                                                                                Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [unavailable,Var_reason_0]),
            error(badarg)
    end;
unavailable(_, _) ->
    error(badarg).
id(#marvin_pdu2_object_guild_unavailable{} = Var_record_0, Var_value_0) ->
    case cloak_validate(id, Var_value_0) of
        {ok,Var_value_1} ->
            cloak_updated(id,
                          Var_record_0#marvin_pdu2_object_guild_unavailable{id =
                                                                                Var_value_1});
        {error,Var_reason_0} ->
            error_logger:error_msg("cloak badarg: field '~s' validation"
                                   " failed with reason: ~p",
                                   [id,Var_reason_0]),
            error(badarg)
    end;
id(_, _) ->
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
cloak_validate(unavailable, Value) when is_boolean(Value) ->
    {ok,Value};
cloak_validate(_, _) ->
    {error,invalid}.
export(#marvin_pdu2_object_guild_unavailable{id = Id,
                                             unavailable = Unavailable}) ->
    #{<<"id">> => Id,<<"unavailable">> => Unavailable}.

