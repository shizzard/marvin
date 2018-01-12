-module(marvin_pdu2_identify_tests).

-include_lib("eunit/include/eunit.hrl").


can_get_new_test() ->
    marvin_pdu2_identify:new(#{
        token => <<"my-token">>,
        compress => true,
        large_threshold => 250,
        properties => #{
            '$os' => <<"linux">>,
            library_name => <<"marvin">>
        },
        shard => [0,1]
    }).


can_get_fields_from_opaque_test() ->
    PDU0 = marvin_pdu2_identify:new(#{
        token => <<"my-token">>,
        compress => true,
        large_threshold => 250,
        properties => #{
            '$os' => <<"linux">>,
            library_name => <<"marvin">>
        },
        shard => [0,1]
    }),
    ?assertEqual(<<"my-token">>, marvin_pdu2_identify:token(PDU0)),
    ?assertEqual(true, marvin_pdu2_identify:compress(PDU0)),
    ?assertEqual(250, marvin_pdu2_identify:large_threshold(PDU0)),
    ?assertMatch({marvin_pdu2_identify_properties, _, _}, marvin_pdu2_identify:properties(PDU0)),
    ?assertMatch({marvin_pdu2_shard, _, _}, marvin_pdu2_identify:shard(PDU0)).


can_set_fields_to_opaque_test() ->
    PDU0 = marvin_pdu2_identify:new(#{
        token => <<"my-token">>,
        compress => true,
        large_threshold => 250,
        properties => #{
            '$os' => <<"linux">>,
            library_name => <<"marvin">>
        },
        shard => [0,1]
    }),
    PDU1 = marvin_pdu2_identify:update(PDU0, #{
        token => <<"my-another-token">>,
        compress => false,
        large_threshold => 150,
        properties => #{'$os' => <<"xunil">>, library_name => <<"nivram">>},
        shard => [1,2]
    }),
    ?assertEqual(<<"my-another-token">>, marvin_pdu2_identify:token(PDU1)),
    ?assertEqual(false, marvin_pdu2_identify:compress(PDU1)),
    ?assertEqual(150, marvin_pdu2_identify:large_threshold(PDU1)),
    ?assertMatch({marvin_pdu2_identify_properties, _, _}, marvin_pdu2_identify:properties(PDU1)),
    ?assertMatch({marvin_pdu2_shard, _, _}, marvin_pdu2_identify:shard(PDU1)).


can_export_valid_opaque_test() ->
    PDU0 = marvin_pdu2_identify:new(#{
        token => <<"my-token">>,
        compress => true,
        large_threshold => 250,
        properties => #{
            '$os' => <<"linux">>,
            library_name => <<"marvin">>
        },
        shard => [0,1]
    }),
    ?assertMatch(#{}, marvin_pdu2_identify:export(PDU0)).


can_get_error_on_invalid_large_threshold_test() ->
    ?assertError(badarg, marvin_pdu2_identify:new(#{
        token => <<"my-token">>,
        compress => true,
        large_threshold => 350,
        properties => #{
            '$os' => <<"linux">>,
            library_name => <<"marvin">>
        },
        shard => [0,1]
    })).


can_get_error_on_invalid_shard_test() ->
    ?assertError(badarg, marvin_pdu2_identify:new(#{
        token => <<"my-token">>,
        compress => true,
        large_threshold => 250,
        properties => #{
            '$os' => <<"linux">>,
            library_name => <<"marvin">>
        },
        shard => [2,1]
    })).


% can_get_valid_parsed_test() ->
%     {ok, JSONBin} = file:read_file(
%         code:priv_dir(marvin_pdu) ++ "/marvin_pdu2_identify_test.json"),
%     ?assertMatch({ok, {?marvin_pdu2_identify(_), undefined}}, marvin_pdu:parse(JSONBin)).
