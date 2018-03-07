-module(marvin_pdu2_dispatch_ready).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guilds :: [marvin_pdu2_object_guild_unavailable:t()],
    presences :: [marvin_pdu2_object_user_presence:t()],
    private_channels :: [marvin_pdu_object_channel_dm:t()],
    relationships :: [marvin_pdu_object_user_relationship:t()],
    session_id :: marvin_pdu2:session_id(),
    shard :: marvin_pdu2_shard:t(),
    user :: marvin_pdu_object_user:t(),
    user_settings :: marvin_pdu_object_user_settings:t(),
    v :: marvin_pdu2:protocol_version(),
    '_trace' :: marvin_pdu2:trace()
}).


-type t() :: #?MODULE{}.
-export_type([t/0]).


%% Interface



cloak_validate(guilds, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_guild_unavailable:new(Item) || Item <- Value]};

cloak_validate(presences, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_user_presence:new(Item) || Item <- Value]};

cloak_validate(private_channels, Value) when is_list(Value) ->
    {ok, [marvin_pdu_object_channel_dm:new(Item) || Item <- Value]};

cloak_validate(relationships, Value) when is_list(Value) ->
    {ok, [marvin_pdu_object_user_relationship:new(Item) || Item <- Value]};

cloak_validate(session_id, Value) when is_binary(Value) ->
    {ok, Value};

cloak_validate(shard, [Shard, TotalShards]) ->
    {ok, marvin_pdu2_shard:new(#{shard => Shard, total_shards => TotalShards})};

cloak_validate(user, Value) ->
    {ok, marvin_pdu2_object_user:new(Value)};

cloak_validate(user_settings, Value) ->
    {ok, marvin_pdu_object_user_settings:new(Value)};

cloak_validate(v, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate('_trace', Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    guilds = Guilds,
    presences = Presences,
    private_channels = PrivateChannels,
    relationships = Relationships,
    session_id = SessionId,
    shard = Shard,
    user = User,
    user_settings = UserSettings,
    v = Version,
    '_trace' = Trace
}) ->
    #{
        <<"guilds">> => [marvin_pdu2_object_guild_unavailable:export(Item) || Item <- Guilds],
        <<"presences">> => [marvin_pdu2_object_user_presence:export(Item) || Item <- Presences],
        <<"private_channels">> => [marvin_pdu_object_channel_dm:export(Item) || Item <- PrivateChannels],
        <<"relationships">> => [marvin_pdu_object_user_relationship:export(Item) || Item <- Relationships],
        <<"session_id">> => SessionId,
        <<"shard">> => [marvin_pdu2_shard:shard(Shard), marvin_pdu2_shard:total_shards(Shard)],
        <<"user">> => marvin_pdu_object_user:export(User),
        <<"user_settings">> => marvin_pdu_object_user_settings:export(UserSettings),
        <<"v">> => Version,
        <<"_trace">> => Trace
    }.
