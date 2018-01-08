-module(marvin_pdu2_hello_tests).

-include_lib("eunit/include/eunit.hrl").


can_get_new_test() ->
    marvin_pdu2_hello:new(#{<<"heartbeat_interval">> => 100, <<"_trace">> => [<<"trace_part">>]}).

can_get_fields_from_opaque_test() ->
    PDU0 = marvin_pdu2_hello:new(#{<<"heartbeat_interval">> => 100, <<"_trace">> => [<<"trace_part">>]}),
    ?assertEqual(100, marvin_pdu2_hello:heartbeat_interval(PDU0)),
    ?assertEqual([<<"trace_part">>], marvin_pdu2_hello:'_trace'(PDU0)).

can_set_fields_to_opaque_test() ->
    PDU0 = marvin_pdu2_hello:new(#{<<"heartbeat_interval">> => 100, <<"_trace">> => [<<"trace_part">>]}),
    PDU1 = marvin_pdu2_hello:heartbeat_interval(PDU0, 200),
    PDU2 = marvin_pdu2_hello:'_trace'(PDU1, [<<"another_trace_part">>]),
    ?assertEqual(200, marvin_pdu2_hello:heartbeat_interval(PDU2)),
    ?assertEqual([<<"another_trace_part">>], marvin_pdu2_hello:'_trace'(PDU2)).

can_export_valid_opaque_test() ->
    PDU0 = marvin_pdu2_hello:new(#{<<"heartbeat_interval">> => 100, <<"_trace">> => [<<"trace_part">>]}),
    ?assertMatch(#{}, marvin_pdu2_hello:export(PDU0)).

can_get_error_on_negative_heatbeat_interval_test() ->
    ?assertError(badarg, marvin_pdu2_hello:new(#{<<"heartbeat_interval">> => -100, <<"_trace">> => [<<"trace_part">>]})).

can_get_error_on_nonstring_trace_part_test() ->
    ?assertError(badarg, marvin_pdu2_hello:new(#{<<"heartbeat_interval">> => 100, <<"_trace">> => ["trace_part"]})).

%%can_get_valid_parsed_test() ->
%%    {ok, JSONBin} = file:read_file(
%%        code:priv_dir(marvin_pdu) ++ "/marvin_pdu2_hello_test.json"),
%%    ?assertMatch({ok, {?marvin_pdu2_hello(_), undefined}}, marvin_pdu:parse(JSONBin)).
