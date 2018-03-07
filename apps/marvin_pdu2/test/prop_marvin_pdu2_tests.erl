-module(prop_marvin_pdu2_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(PROPTEST(A), ?_assert(proper:quickcheck(A))).


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
        marvin_pdu2_dispatch_resumed
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
        _ ->
            throw(not_same_pdus)
    end.


%% Generators


marvin_pdu2_heartbeat() ->
    ?LET(Sequence, marvin_pdu2:sequence(), #{plain_value => Sequence}).

marvin_pdu2_identify_properties() ->
    ?LET(
        {OS, Browser, Device, Referrer, ReferringDomain},
        {
            non_empty(marvin_pdu2:properties_os()),
            non_empty(marvin_pdu2:properties_browser()),
            non_empty(marvin_pdu2:properties_device()),
            non_empty(marvin_pdu2:properties_referrer()),
            non_empty(marvin_pdu2:properties_referring_domain())
        },
        #{
            '$os' => OS,
            '$browser' => Browser,
            '$device' => Device,
            '$referrer' => Referrer,
            '$referring_domain' => ReferringDomain
        }
    ).

marvin_pdu2_identify() ->
    ?LET(
        {Token, Compress, LargeThreshold, Properties, Shard},
        {
            non_empty(marvin_pdu2:token()),
            marvin_pdu2:compress(),
            marvin_pdu2:large_threshold(),
            marvin_pdu2_identify_properties(),
            ?SUCHTHAT(
                [Shard, TotalShards],
                [marvin_pdu2:shard(), marvin_pdu2:total_shards()],
                Shard < TotalShards
            )
        },
        #{
            token => Token,
            compress => Compress,
            large_threshold => LargeThreshold,
            properties => Properties,
            shard => Shard
        }
    ).

marvin_pdu2_resume() ->
    ?LET(
        {Token, SessionId, Sequence},
        {
            non_empty(marvin_pdu2:token()),
            non_empty(marvin_pdu2:session_id()),
            marvin_pdu2:sequence()
        },
        #{
            token => Token,
            session_id => SessionId,
            seq => Sequence
        }
    ).

marvin_pdu2_hello() ->
    ?LET(
        {HeartbeatInterval, Trace},
        {
            non_empty(marvin_pdu2:heartbeat_interval()),
            list(non_empty(binary()))
        },
        #{
            heartbeat_interval => HeartbeatInterval,
            '_trace' => Trace
        }
    ).

marvin_pdu2_invalid_session() ->
    #{}.

marvin_pdu2_heartbeat_ack() ->
    #{}.

marvin_pdu2_dispatch_resumed() ->
    ?LET(
        Trace,
        list(non_empty(binary())),
        #{
            '_trace' => Trace
        }
    ).
