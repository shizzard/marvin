-module(marvin_pdu_identify_tests).

-include_lib("eunit/include/eunit.hrl").
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").



t01_can_get_new_test() ->
    marvin_pdu_identify:new(
        <<"my-token">>, <<"linux">>, <<"marvin">>, true, 250, [0,1]
    ).


t02_can_get_fields_from_opaque_test() ->
    PDU0 = marvin_pdu_identify:new(
        <<"my-token">>, <<"linux">>, <<"marvin">>, true, 250, [0,1]
    ),
    ?assertEqual(<<"my-token">>, marvin_pdu_identify:token(PDU0)),
    ?assertEqual(<<"linux">>, marvin_pdu_identify:properties_os(PDU0)),
    ?assertEqual(<<"marvin">>, marvin_pdu_identify:properties_library_name(PDU0)),
    ?assertEqual(true, marvin_pdu_identify:compress(PDU0)),
    ?assertEqual(250, marvin_pdu_identify:large_threshold(PDU0)),
    ?assertEqual([0,1], marvin_pdu_identify:shard(PDU0)).


t03_can_set_fields_to_opaque_test() ->
    PDU0 = marvin_pdu_identify:new(
        <<"my-token">>, <<"linux">>, <<"marvin">>, true, 250, [0,1]
    ),
    PDU1 = marvin_pdu_identify:token(<<"my-another-token">>, PDU0),
    PDU2 = marvin_pdu_identify:properties_os(<<"windows">>, PDU1),
    PDU3 = marvin_pdu_identify:properties_library_name(<<"lambada">>, PDU2),
    PDU4 = marvin_pdu_identify:compress(false, PDU3),
    PDU5 = marvin_pdu_identify:large_threshold(150, PDU4),
    PDU6 = marvin_pdu_identify:shard([1,2], PDU5),
    ?assertEqual(<<"my-another-token">>, marvin_pdu_identify:token(PDU6)),
    ?assertEqual(<<"windows">>, marvin_pdu_identify:properties_os(PDU6)),
    ?assertEqual(<<"lambada">>, marvin_pdu_identify:properties_library_name(PDU6)),
    ?assertEqual(false, marvin_pdu_identify:compress(PDU6)),
    ?assertEqual(150, marvin_pdu_identify:large_threshold(PDU6)),
    ?assertEqual([1,2], marvin_pdu_identify:shard(PDU6)).


t04_can_render_valid_opaque_test() ->
    PDU0 = marvin_pdu_identify:new(
        <<"my-token">>, <<"linux">>, <<"marvin">>, true, 250, [0,1]
    ),
    ?assertMatch({ok, _Message}, marvin_pdu:render(PDU0)).


t05_can_get_error_on_invalid_large_threshold_test() ->
    PDU0 = marvin_pdu_identify:new(
        <<"my-token">>, <<"linux">>, <<"marvin">>, true, 350, [0,1]
    ),
    ?assertMatch(
        {error, {validation_errors, [{_, _, [_, ?discord_key_identify_large_threshold]}]}},
        marvin_pdu:render(PDU0)
    ).


t06_can_get_error_on_invalid_shard_test() ->
    PDU0 = marvin_pdu_identify:new(
        <<"my-token">>, <<"linux">>, <<"marvin">>, true, 250, [3,2]
    ),
    ?assertMatch(
        {error, {validation_errors, [{_, _, [_, ?discord_key_identify_shard]}]}},
        marvin_pdu:render(PDU0)
    ).


t99_can_get_valid_parsed_test() ->
    {ok, JSONBin} = file:read_file(
        code:priv_dir(marvin_pdu) ++ "/marvin_pdu_identify_test.json"),
    ?assertMatch({ok, ?marvin_pdu_identify(_)}, marvin_pdu:parse(JSONBin)).
