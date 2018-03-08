-module(prop_marvin_pdu2_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(PROPTEST(A), ?_assert(proper:quickcheck(A, [{to_file, user}]))).
-define(MAP(Typedef), ?LET(Proplist, Typedef, maps:from_list(lists:flatten(Proplist)))).
-define(OPTIONAL(FieldTypedef), oneof([[FieldTypedef], []])).


%% Tests


proper_test_() ->
    [?PROPTEST(proper_test_for_pdu_mod(PDUMod)) || PDUMod <- pdu_mods()].


proper_test_for_pdu_mod(PDUMod) ->
    ?FORALL(PDU, erlang:apply(?MODULE, PDUMod, []), is_valid(PDU, PDUMod)).


%% Internals


pdu_mods() ->
    [
        marvin_pdu2_heartbeat,
        marvin_pdu2_heartbeat_ack,
        marvin_pdu2_identify_properties,
        marvin_pdu2_identify,
        marvin_pdu2_resume,
        marvin_pdu2_hello,
        marvin_pdu2_invalid_session,
        marvin_pdu2_dispatch_ready,
        marvin_pdu2_dispatch_resumed,

        marvin_pdu2_object_user,
        marvin_pdu2_object_guild_unavailable,
        marvin_pdu2_object_channel_dm
    ].


is_valid(PDU, PDUMod) ->
    try
        NewPDU = PDUMod:export(PDUMod:new(PDU)),
        ensure_same_pdus(PDU, NewPDU),
        true
    catch
        throw:badarg ->
            false;
        throw:not_same_pdus ->
            false
    end.


ensure_same_pdus(OriginalPDU, ResultPDU) ->
    maps:fold(fun ensure_same_pdus_fold/3, ResultPDU, OriginalPDU).


ensure_same_pdus_fold(Key, Value, ResultPDU) ->
    case maps:get(list_to_binary(atom_to_list(Key)), ResultPDU) of
        Value ->
            ResultPDU;
        ReturnValue when is_map(ReturnValue) ->
            maps:fold(fun ensure_same_pdus_fold/3, ReturnValue, Value),
            ResultPDU;
        ReturnValue when is_list(ReturnValue) ->
            % should fix this eventually
            ResultPDU;
        _ ->
            throw(not_same_pdus)
    end.


%% Generators - PDUs


marvin_pdu2_heartbeat() ->
    ?MAP([{plain_value, marvin_pdu2:sequence()}]).

marvin_pdu2_identify_properties() ->
    ?MAP([
        {'$os', non_empty(marvin_pdu2:properties_os())},
        {'$browser', non_empty(marvin_pdu2:properties_browser())},
        {'$device', non_empty(marvin_pdu2:properties_device())},
        {'$referrer', non_empty(marvin_pdu2:properties_referrer())},
        {'$referring_domain', non_empty(marvin_pdu2:properties_referring_domain())}
    ]).

marvin_pdu2_identify() ->
    ?MAP([
        {token, non_empty(marvin_pdu2:token())},
        {compress, marvin_pdu2:compress()},
        {large_threshold, marvin_pdu2:large_threshold()},
        {properties, marvin_pdu2_identify_properties()},
        {shard, ?SUCHTHAT(
            [Shard, TotalShards],
            [marvin_pdu2:shard(), marvin_pdu2:total_shards()],
            Shard < TotalShards
        )}
    ]).

marvin_pdu2_resume() ->
    ?MAP([
        {token, non_empty(marvin_pdu2:token())},
        {session_id, non_empty(marvin_pdu2:session_id())},
        {seq, marvin_pdu2:sequence()}
    ]).

marvin_pdu2_hello() ->
    ?MAP([
        {heartbeat_interval, non_empty(marvin_pdu2:heartbeat_interval())},
        {'_trace', list(non_empty(marvin_pdu2:trace_part()))}
    ]).

marvin_pdu2_invalid_session() ->
    #{}.

marvin_pdu2_heartbeat_ack() ->
    #{}.


marvin_pdu2_dispatch_ready() ->
    ?MAP([
        {guilds, list(marvin_pdu2_object_guild_unavailable())},
        {private_channels, list(marvin_pdu2_object_channel_dm())},
        {session_id, non_empty(marvin_pdu2:session_id())},
        {user, marvin_pdu2_object_user()},
        {v, marvin_pdu2:protocol_version()},
        {'_trace', list(non_empty(marvin_pdu2:trace_part()))}
    ]).

marvin_pdu2_dispatch_resumed() ->
    ?MAP([
        {'_trace', list(non_empty(marvin_pdu2:trace_part()))}
    ]).


%% Generators - Objects


marvin_pdu2_object_user() ->
    ?MAP([
        {id, non_empty(marvin_pdu2:snowflake())},
        {username, non_empty(marvin_pdu2:username())},
        {discriminator, non_empty(marvin_pdu2:discriminator())},
        {avatar, non_empty(marvin_pdu2:avatar())},
        ?OPTIONAL({bot, marvin_pdu2:bot()}),
        ?OPTIONAL({mfa_enabled, marvin_pdu2:mfa_enabled()}),
        ?OPTIONAL({verified, marvin_pdu2:verified()}),
        ?OPTIONAL({email, non_empty(marvin_pdu2:email())})
    ]).

marvin_pdu2_object_guild_unavailable() ->
    ?MAP([
        {id, non_empty(marvin_pdu2:snowflake())},
        {unavailable, marvin_pdu2:unavailable()}
    ]).

marvin_pdu2_object_channel_dm() ->
    ?MAP([
        {id, non_empty(marvin_pdu2:snowflake())},
        {type, 1},
        {last_message_id, non_empty(marvin_pdu2:snowflake())},
        {recipients, list(marvin_pdu2_object_user())}
    ]).
