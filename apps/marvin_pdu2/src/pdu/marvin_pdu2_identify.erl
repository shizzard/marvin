-module(marvin_pdu2_identify).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    token :: token(),
    compress :: compress(),
    large_threshold :: large_threshold(),
    properties :: properties(),
    shard :: shard(),
    prot_shard :: prot_shard(),
    prot_total_shards :: prot_total_shards()
}).

-type token() :: marvin_pdu2:token().
-type compress() :: boolean().
-type large_threshold() :: 50..250.
-type properties() :: marvin_pdu2_identify_properties:t().
-type shard() :: marvin_pdu2:shard_spec().
-type prot_shard() :: marvin_pdu2:shard().
-type prot_total_shards() :: marvin_pdu2:total_shards().
-type t() :: #?MODULE{}.

-export_type([
    token/0, compress/0, large_threshold/0, properties/0, shard/0,
    prot_shard/0, prot_total_shards/0, t/0
]).


cloak_validate(token, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(compress, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(large_threshold, Value) when Value > 0 andalso Value =< 250 ->
    {ok, Value};

cloak_validate(properties, Value) ->
    {ok, marvin_pdu2_identify_properties:new(Value)};

cloak_validate(shard, [Shard, TotalShards] = Value)
when is_integer(Shard) andalso is_integer(TotalShards)
andalso Shard < TotalShards ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


cloak_validate_struct(#?MODULE{shard = [Shard, TotalShards]} = Struct) ->
    {ok, Struct#?MODULE{prot_shard = Shard, prot_total_shards = TotalShards}};

cloak_validate_struct(_Struct) ->
    {error, invalid}.


export(#?MODULE{
    token = Token,
    compress = Compress,
    large_threshold = LargeThreshold,
    properties = Properties,
    shard = ShardSpec
}) ->
    #{
        <<"token">> => Token,
        <<"compress">> => Compress,
        <<"large_threshold">> => LargeThreshold,
        <<"properties">> => marvin_pdu2_identify_properties:export(Properties),
        <<"shard">> => ShardSpec
    }.
