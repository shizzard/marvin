-module(marvin_pdu_identify).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, new/6, data_map/0, export/1]).
-export([
    properties_os/1, properties_os/2, properties_library_name/1, properties_library_name/2,
    token/1, token/2, compress/1, compress/2, large_threshold/1, large_threshold/2, shard/1, shard/2
]).



%% Types



-type properties_os() :: binary().
-type properties_library_name() :: binary().
-type properties_browser() :: properties_library_name().
-type properties_device() :: properties_library_name().
-type properties_referrer() :: binary().
-type properties_referring_domain() :: binary().
-type token() :: binary().
-type compress() :: boolean().
-type large_threshold() :: non_neg_integer().
-type shard() :: [non_neg_integer(), ...].
-export_type([
    properties_os/0, properties_library_name/0, properties_browser/0,
    properties_device/0, properties_referrer/0, properties_referring_domain/0,
    token/0, properties/0, compress/0, large_threshold/0, shard_seq/0, shard/0
]).

-record(properties, {
    os :: properties_os(),
    library_name :: properties_library_name()
}).
-type properties() :: #properties{}.
-record(pdu, {
    token :: token(),
    properties :: properties(),
    compress :: compress(),
    large_threshold :: large_threshold(),
    shard :: shard()
}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_identify(PDU :: pdu_internal()).



%% Interface



-spec new(Data :: #{}) ->
    Ret :: pdu().

new(#{
    ?discord_key_identify_token := Token,
    ?discord_key_identify_properties := #{
        ?discord_key_identify_properties_os := OS,
        ?discord_key_identify_properties_browser := LibraryName,
        ?discord_key_identify_properties_device := LibraryName,
        ?discord_key_identify_properties_referrer := <<"">>,
        ?discord_key_identify_properties_referring_domain := <<"">>
    },
    ?discord_key_identify_compress := Compress,
    ?discord_key_identify_large_threshold := LargeThreshold,
    ?discord_key_identify_shard := Shard
}) ->
    new(Token, OS, LibraryName, Compress, LargeThreshold, Shard).



-spec new(
    Token :: token(),
    OS :: properties_os(),
    LibraryName :: properties_library_name(),
    Compress :: compress(),
    LargeThreshold :: large_threshold(),
    Shard :: shard()
) ->
    Ret :: pdu().

new(Token, OS, LibraryName, Compress, LargeThreshold, Shard) ->
    ?marvin_pdu_identify(#pdu{
        token = Token,
        properties = #properties{
            os = OS, library_name = LibraryName
        },
        compress = Compress,
        large_threshold = LargeThreshold,
        shard = Shard
    }).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_identify_token, required, jiffy_vm:string()),
        jiffy_vm:hashfield(?discord_key_identify_properties, required, jiffy_vm:hash([
            jiffy_vm:hashfield(?discord_key_identify_properties_os, required, jiffy_vm:string()),
            jiffy_vm:hashfield(?discord_key_identify_properties_browser, required, jiffy_vm:string()),
            jiffy_vm:hashfield(?discord_key_identify_properties_device, required, jiffy_vm:string()),
            jiffy_vm:hashfield(?discord_key_identify_properties_referrer, required, jiffy_vm:string(fun validate_properties_refs/3)),
            jiffy_vm:hashfield(?discord_key_identify_properties_referring_domain, required, jiffy_vm:string(fun validate_properties_refs/3))
        ]), fun validate_properties/3),
        jiffy_vm:hashfield(?discord_key_identify_compress, required, jiffy_vm:boolean()),
        jiffy_vm:hashfield(?discord_key_identify_large_threshold, required, jiffy_vm:integer(fun validate_large_threshold/3)),
        jiffy_vm:hashfield(?discord_key_identify_shard, required, jiffy_vm:list([jiffy_vm:integer(fun validate_shard_element/3)], fun validate_shard/3))
    ]).



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_identify(#pdu{
    token = Token,
    properties = #properties{
        os = OS, library_name = LibraryName
    },
    compress = Compress,
    large_threshold = LargeThreshold,
    shard = Shard
})) ->
    {ok, #{
        ?discord_key_identify_token => Token,
        ?discord_key_identify_properties => #{
            ?discord_key_identify_properties_os => OS,
            ?discord_key_identify_properties_browser => LibraryName,
            ?discord_key_identify_properties_device => LibraryName,
            ?discord_key_identify_properties_referrer => <<"">>,
            ?discord_key_identify_properties_referring_domain => <<"">>
        },
        ?discord_key_identify_compress => Compress,
        ?discord_key_identify_large_threshold => LargeThreshold,
        ?discord_key_identify_shard => Shard
    }}.



-spec properties_os(PDU :: pdu()) ->
    Ret :: properties_os().

properties_os(?marvin_pdu_identify(#pdu{properties = #properties{os = Value}})) ->
    Value.



-spec properties_os(OS :: properties_os(), PDU :: pdu()) ->
    Ret :: pdu().

properties_os(Value, ?marvin_pdu_identify(#pdu{properties = Properties} = PDU)) ->
    ?marvin_pdu_identify(PDU#pdu{properties = Properties#properties{os = Value}}).



-spec properties_library_name(PDU :: pdu()) ->
    Ret :: properties_library_name().

properties_library_name(?marvin_pdu_identify(#pdu{properties = #properties{library_name = Value}})) ->
    Value.



-spec properties_library_name(OS :: properties_library_name(), PDU :: pdu()) ->
    Ret :: pdu().

properties_library_name(Value, ?marvin_pdu_identify(#pdu{properties = Properties} = PDU)) ->
    ?marvin_pdu_identify(PDU#pdu{properties = Properties#properties{library_name = Value}}).



-spec token(PDU :: pdu()) ->
    Ret :: token().

token(?marvin_pdu_identify(#pdu{token = Value})) ->
    Value.



-spec token(OS :: token(), PDU :: pdu()) ->
    Ret :: pdu().

token(Value, ?marvin_pdu_identify(#pdu{} = PDU)) ->
    ?marvin_pdu_identify(PDU#pdu{token = Value}).



-spec compress(PDU :: pdu()) ->
    Ret :: compress().

compress(?marvin_pdu_identify(#pdu{compress = Value})) ->
    Value.



-spec compress(OS :: compress(), PDU :: pdu()) ->
    Ret :: pdu().

compress(Value, ?marvin_pdu_identify(#pdu{} = PDU)) ->
    ?marvin_pdu_identify(PDU#pdu{compress = Value}).



-spec large_threshold(PDU :: pdu()) ->
    Ret :: large_threshold().

large_threshold(?marvin_pdu_identify(#pdu{large_threshold = Value})) ->
    Value.



-spec large_threshold(OS :: large_threshold(), PDU :: pdu()) ->
    Ret :: pdu().

large_threshold(Value, ?marvin_pdu_identify(#pdu{} = PDU)) ->
    ?marvin_pdu_identify(PDU#pdu{large_threshold = Value}).



-spec shard(PDU :: pdu()) ->
    Ret :: shard().

shard(?marvin_pdu_identify(#pdu{shard = Value})) ->
    Value.



-spec shard(OS :: shard(), PDU :: pdu()) ->
    Ret :: pdu().

shard(Value, ?marvin_pdu_identify(#pdu{} = PDU)) ->
    ?marvin_pdu_identify(PDU#pdu{shard = Value}).



%% Internals



-spec ?validator_spec(validate_properties_refs).

validate_properties_refs(validate, _, _Refs) when _Refs =/= <<"">> ->
    {error, <<"Should be empty string">>};

validate_properties_refs(validate, _, _Refs) ->
    {ok, valid};

validate_properties_refs(fix, _, _) ->
    {error, invalid}.



-spec ?validator_spec(validate_properties).

validate_properties(validate, _, #{
    ?discord_key_identify_properties_browser := _SomeLibraryName,
    ?discord_key_identify_properties_device := _AnotherLibraryName
}) when _SomeLibraryName =/= _AnotherLibraryName ->
    {error, <<"Browser and device properties should be equal">>};

validate_properties(validate, _, #{
    ?discord_key_identify_properties_browser := _SameLibraryName,
    ?discord_key_identify_properties_device := _SameLibraryName
}) when _SameLibraryName == <<"">> ->
    {error, <<"Browser and device properties should not be empty">>};

validate_properties(validate, _, _Value) ->
    {ok, valid};

validate_properties(fix, _, _Value) ->
    {error, invalid}.



-spec ?validator_spec(validate_large_threshold).

validate_large_threshold(validate, _, Value)
when (Value < 50) orelse (Value > 250) ->
    {error, <<"Out of bounds (50..250)">>};

validate_large_threshold(validate, _, _) ->
    {ok, valid};

validate_large_threshold(fix, _, _) ->
    {error, invalid}.



-spec ?validator_spec(validate_shard_element).

validate_shard_element(validate, _, Value) when Value < 0 ->
    {error, <<"Should be positive">>};

validate_shard_element(validate, _, _) ->
    {ok, valid};

validate_shard_element(fix, _, _) ->
    {error, invalid}.



-spec ?validator_spec(validate_shard).

validate_shard(validate, _, Value)
when length(Value) =/= 2 ->
    {error, <<"Should be two-list of integers">>};

validate_shard(validate, _, [ShardId, OfShards])
when ShardId >= OfShards ->
    {error, <<"ShardId parameter is out of bounds (0..ShardId..OfShards)">>};

validate_shard(validate, _, _) ->
    {ok, valid};

validate_shard(fix, _, _) ->
    {error, invalid}.
