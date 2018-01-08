-module(marvin_pdu2_heartbeat_ack_tests).

-include_lib("eunit/include/eunit.hrl").


can_get_new_test() ->
    marvin_pdu2_heartbeat_ack:new(#{}).


can_render_valid_opaque_test() ->
    PDU0 = marvin_pdu2_heartbeat_ack:new(#{}),
    ?assertMatch(undefined, marvin_pdu2_heartbeat_ack:export(PDU0)).


% can_get_valid_parsed_test() ->
%     {ok, JSONBin} = file:read_file(
%         code:priv_dir(marvin_pdu) ++ "/marvin_pdu2_heartbeat_ack_test.json"),
%     ?assertMatch({ok, {?marvin_pdu2_heartbeat_ack(_), undefined}}, marvin_pdu:parse(JSONBin)).
