-module(marvin_pdu2_shard_tests).

-include_lib("eunit/include/eunit.hrl").


can_get_new_test() ->
    marvin_pdu2_shard:new(#{shard => 1, total_shards => 2}).


can_get_fields_from_opaque_test() ->
    PDU0 = marvin_pdu2_shard:new(#{shard => 1, total_shards => 2}),
    ?assertEqual(1, marvin_pdu2_shard:shard(PDU0)),
    ?assertEqual(2, marvin_pdu2_shard:total_shards(PDU0)).


can_set_fields_to_opaque_test() ->
    PDU0 = marvin_pdu2_shard:new(#{shard => 1, total_shards => 2}),
    PDU1 = marvin_pdu2_shard:update(PDU0, #{shard => 2, total_shards => 3}),
    ?assertEqual(2, marvin_pdu2_shard:shard(PDU1)),
    ?assertEqual(3, marvin_pdu2_shard:total_shards(PDU1)).


can_get_error_on_invalid_shard_test() ->
    ?assertError(badarg, marvin_pdu2_shard:new(#{shard => -100, total_shards => 3})).


can_get_error_on_invalid_total_shards_test() ->
    ?assertError(badarg, marvin_pdu2_shard:new(#{shard => 1, total_shards => -3})).


can_get_error_on_invalid_shards_config_test() ->
    ?assertError(badarg, marvin_pdu2_shard:new(#{shard => 3, total_shards => 2})).


can_export_valid_opaque_test() ->
    PDU0 = marvin_pdu2_shard:new(#{shard => 1, total_shards => 2}),
    ?assertMatch(#{}, marvin_pdu2_shard:export(PDU0)).
