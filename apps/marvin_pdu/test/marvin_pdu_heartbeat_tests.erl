-module(marvin_pdu_heartbeat_tests).

-include_lib("eunit/include/eunit.hrl").
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").



t01_can_get_new_test() ->
    marvin_pdu_heartbeat:new(100).


t02_can_get_fields_from_opaque_test() ->
    PDU0 = marvin_pdu_heartbeat:new(100),
    ?assertEqual(100, marvin_pdu_heartbeat:last_seq(PDU0)).


t03_can_set_fields_to_opaque_test() ->
    PDU0 = marvin_pdu_heartbeat:new(100),
    PDU1 = marvin_pdu_heartbeat:last_seq(200, PDU0),
    ?assertEqual(200, marvin_pdu_heartbeat:last_seq(PDU1)).


t04_can_render_valid_opaque_test() ->
    PDU0 = marvin_pdu_heartbeat:new(100),
    ?assertMatch({ok, _Message}, marvin_pdu:render(PDU0)).


t05_can_get_error_on_negative_last_seq_test() ->
    PDU0 = marvin_pdu_heartbeat:new(-100),
    ?assertMatch(
        {error, {validation_errors, [{_, _, [_]}]}},
        marvin_pdu:render(PDU0)
    ).


t99_can_get_valid_parsed_test() ->
    {ok, JSONBin} = file:read_file(
        code:priv_dir(marvin_pdu) ++ "/marvin_pdu_heartbeat_test.json"),
    ?assertMatch({ok, ?marvin_pdu_heartbeat(_)}, marvin_pdu:parse(JSONBin)).
