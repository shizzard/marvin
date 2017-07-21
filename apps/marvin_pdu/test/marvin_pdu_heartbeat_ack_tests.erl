-module(marvin_pdu_heartbeat_ack_tests).

-include_lib("eunit/include/eunit.hrl").
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").



t01_can_get_new_test() ->
    marvin_pdu_heartbeat_ack:new().


t04_can_render_valid_opaque_test() ->
    PDU0 = marvin_pdu_heartbeat_ack:new(100),
    ?assertMatch({ok, _Message}, marvin_pdu:render(PDU0)).


t99_can_get_valid_parsed_test() ->
    {ok, JSONBin} = file:read_file(
        code:priv_dir(marvin_pdu) ++ "/marvin_pdu_heartbeat_ack_test.json"),
    ?assertMatch({ok, ?marvin_pdu_heartbeat_ack(_)}, marvin_pdu:parse(JSONBin)).
