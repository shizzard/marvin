-module(marvin_pdu2_heartbeat_tests).

-include_lib("eunit/include/eunit.hrl").


can_get_new_test() ->
    marvin_pdu2_heartbeat:new(#{last_seq => 100}).


can_get_fields_from_opaque_test() ->
    PDU0 = marvin_pdu2_heartbeat:new(#{last_seq => 100}),
    ?assertEqual(100, marvin_pdu2_heartbeat:last_seq(PDU0)).


can_set_fields_to_opaque_test() ->
    PDU0 = marvin_pdu2_heartbeat:new(#{last_seq => 100}),
    PDU1 = marvin_pdu2_heartbeat:last_seq(PDU0, 200),
    ?assertEqual(200, marvin_pdu2_heartbeat:last_seq(PDU1)).


can_export_valid_opaque_test() ->
    PDU0 = marvin_pdu2_heartbeat:new(#{last_seq => 100}),
    ?assertMatch(100, marvin_pdu2_heartbeat:export(PDU0)).


can_get_error_on_negative_last_seq_test() ->
    ?assertError(badarg, marvin_pdu2_heartbeat:new(#{last_seq => -100})).


%%can_get_valid_parsed_test() ->
%%    {ok, JSONBin} = file:read_file(
%%        code:priv_dir(marvin_pdu) ++ "/marvin_pdu2_heartbeat_test.json"),
%%    ?assertMatch({ok, {?marvin_pdu2_heartbeat(_), undefined}}, marvin_pdu:parse(JSONBin)).
