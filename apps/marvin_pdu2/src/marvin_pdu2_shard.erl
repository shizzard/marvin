-module(marvin_pdu2_shard).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    shard :: non_neg_integer(),
    total_shards :: non_neg_integer()
}).

-type t() :: #?MODULE{}.
-export_type([t/0]).


cloak_validate(shard, Value) when Value >= 0 ->
    {ok, Value};

cloak_validate(total_shards, Value) when Value > 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


cloak_validate_struct(#?MODULE{shard = Shard, total_shards = TotalShards} = Struct)
when Shard < TotalShards ->
    {ok, Struct};

cloak_validate_struct(_) ->
    {error, invalid}.


export(#?MODULE{shard = Shard, total_shards = TotalShards}) ->
    #{shard => Shard, total_shards => TotalShards}.
