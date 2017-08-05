-module(marvin_pdu_dispatch_ready).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, new/9, data_map/0, export/1]).
-export([
    guilds/1, presences/1, private_channels/1, relationships/1, session_id/1,
    shard/1, user/1, user_settings/1, v/1,
    guilds/2, presences/2, private_channels/2, relationships/2, session_id/2,
    shard/2, user/2, user_settings/2, v/2
]).



%% Types


-type guilds() :: [maprin_pdu_object_guild_unavailable:object()].
-type presences() :: [].
-type private_channels() :: [marvin_pdu_object_channel_dm:object()].
-type relationships() :: [].
-type session_id() :: non_neg_integer().
-type shard() :: [non_neg_integer()].
-type user() :: marvin_pdu_object_user:object().
-type user_settings() :: #{}.
-type v() :: non_neg_integer().
-export_type([
    guilds/0, presences/0, private_channels/0, relationships/0, session_id/0,
    shard/0, user/0, user_settings/0, v/0
]).
-record(pdu, {
    guilds :: guilds(),
    presences :: presences(),
    private_channels :: private_channels(),
    relationships :: relationships(),
    session_id :: session_id(),
    shard :: shard(),
    user :: user(),
    user_settings :: user_settings(),
    v :: v()
}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_dispatch_ready(PDU :: pdu_internal()).



%% Interface



-spec new(Data :: #{}) ->
    Ret :: pdu().

new(#{
    ?discord_key_dispatch_ready_guilds := Guilds,
    ?discord_key_dispatch_ready_presences := Presences,
    ?discord_key_dispatch_ready_private_channels := PrivateChannels,
    ?discord_key_dispatch_ready_relationships := Relationships,
    ?discord_key_dispatch_ready_session_id := SessionId,
    ?discord_key_dispatch_ready_shard := Shard,
    ?discord_key_dispatch_ready_user := User,
    ?discord_key_dispatch_ready_user_settings := UserSettings,
    ?discord_key_dispatch_ready_v := V
}) ->
    GuildObjects = [marvin_pdu_object_guild_unavailable:new(Guild) || Guild <- Guilds],
    PrivateChannelObjects = [marvin_pdu_object_channel_dm:new(PrivateChannel) || PrivateChannel <- PrivateChannels],
    UserObject = marvin_pdu_object_user:new(User),
    new(GuildObjects, Presences, PrivateChannelObjects, Relationships, SessionId, Shard, UserObject, UserSettings, V).



-spec new(
    Guilds :: guilds(),
    Presences :: presences(),
    PrivateChannels :: private_channels(),
    Relationships :: relationships(),
    SessionId :: session_id(),
    Shard :: shard(),
    User :: user(),
    UserSettings :: user_settings(),
    V :: v()
) ->
    Ret :: pdu().

new(Guilds, Presences, PrivateChannels, Relationships, SessionId, Shard, User, UserSettings, V) ->
    ?marvin_pdu_dispatch_ready(#pdu{
        guilds = Guilds,
        presences = Presences,
        private_channels = PrivateChannels,
        relationships = Relationships,
        session_id = SessionId,
        shard = Shard,
        user = User,
        user_settings = UserSettings,
        v = V
    }).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_dispatch_ready_guilds, required, jiffy_vm:list(
            [marvin_pdu_object_guild_unavailable:data_map()]
        )),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_presences, required, jiffy_vm:any()),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_private_channels, required, jiffy_vm:list(
            [marvin_pdu_object_channel_dm:data_map()]
        )),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_relationships, required, jiffy_vm:any()),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_session_id, required, jiffy_vm:string()),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_shard, required, jiffy_vm:list([jiffy_vm:integer(fun validate_shard_element/3)], fun validate_shard/3)),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_user, required, marvin_pdu_object_user:data_map()),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_user_settings, required, jiffy_vm:hash([])),
        jiffy_vm:hashfield(?discord_key_dispatch_ready_v, required, jiffy_vm:integer(fun validate_version/3))
    ]).



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_dispatch_ready(#pdu{
    guilds = GuildObjects,
    presences = Presences,
    private_channels = PrivateChannelObjects,
    relationships = Relationships,
    session_id = SessionId,
    shard = Shard,
    user = UserObject,
    user_settings = UserSettings,
    v = V
})) ->
    Guilds = lists:map(fun(GuildObject) ->
        {ok, Guild} = marvin_pdu_object_guild_unavailable:export(GuildObject),
        Guild
    end, GuildObjects),
    PrivateChannels = lists:map(fun(PrivateChannelObject) ->
        {ok, PrivateChannel} = marvin_pdu_object_channel_dm:export(PrivateChannelObject),
        PrivateChannel
    end, PrivateChannelObjects),
    {ok, User} = marvin_pdu_object_user:export(UserObject),
    {ok, #{
        ?discord_key_dispatch_ready_guilds => Guilds,
        ?discord_key_dispatch_ready_presences => Presences,
        ?discord_key_dispatch_ready_private_channels => PrivateChannels,
        ?discord_key_dispatch_ready_relationships => Relationships,
        ?discord_key_dispatch_ready_session_id => SessionId,
        ?discord_key_dispatch_ready_shard => Shard,
        ?discord_key_dispatch_ready_user => User,
        ?discord_key_dispatch_ready_user_settings => UserSettings,
        ?discord_key_dispatch_ready_v => V
    }}.



-spec guilds(PDU :: pdu()) ->
    Ret :: guilds().

guilds(?marvin_pdu_dispatch_ready(#pdu{guilds = Value})) ->
    Value.



-spec guilds(Value :: guilds(), PDU :: pdu()) ->
    Ret :: pdu().

guilds(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{guilds = Value}).



-spec presences(PDU :: pdu()) ->
    Ret :: presences().

presences(?marvin_pdu_dispatch_ready(#pdu{presences = Value})) ->
    Value.



-spec presences(Value :: presences(), PDU :: pdu()) ->
    Ret :: pdu().

presences(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{presences = Value}).



-spec private_channels(PDU :: pdu()) ->
    Ret :: private_channels().

private_channels(?marvin_pdu_dispatch_ready(#pdu{private_channels = Value})) ->
    Value.



-spec private_channels(Value :: private_channels(), PDU :: pdu()) ->
    Ret :: pdu().

private_channels(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{private_channels = Value}).



-spec relationships(PDU :: pdu()) ->
    Ret :: relationships().

relationships(?marvin_pdu_dispatch_ready(#pdu{relationships = Value})) ->
    Value.



-spec relationships(Value :: relationships(), PDU :: pdu()) ->
    Ret :: pdu().

relationships(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{relationships = Value}).



-spec session_id(PDU :: pdu()) ->
    Ret :: session_id().

session_id(?marvin_pdu_dispatch_ready(#pdu{session_id = Value})) ->
    Value.



-spec session_id(Value :: session_id(), PDU :: pdu()) ->
    Ret :: pdu().

session_id(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{session_id = Value}).



-spec shard(PDU :: pdu()) ->
    Ret :: shard().

shard(?marvin_pdu_dispatch_ready(#pdu{shard = Value})) ->
    Value.



-spec shard(Value :: shard(), PDU :: pdu()) ->
    Ret :: pdu().

shard(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{shard = Value}).



-spec user(PDU :: pdu()) ->
    Ret :: user().

user(?marvin_pdu_dispatch_ready(#pdu{user = Value})) ->
    Value.



-spec user(Value :: user(), PDU :: pdu()) ->
    Ret :: pdu().

user(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{user = Value}).



-spec user_settings(PDU :: pdu()) ->
    Ret :: user_settings().

user_settings(?marvin_pdu_dispatch_ready(#pdu{user_settings = Value})) ->
    Value.



-spec user_settings(Value :: user_settings(), PDU :: pdu()) ->
    Ret :: pdu().

user_settings(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{user_settings = Value}).



-spec v(PDU :: pdu()) ->
    Ret :: v().

v(?marvin_pdu_dispatch_ready(#pdu{v = Value})) ->
    Value.



-spec v(Value :: v(), PDU :: pdu()) ->
    Ret :: pdu().

v(Value, ?marvin_pdu_dispatch_ready(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_ready(PDU#pdu{v = Value}).



%% Validators



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



-spec ?validator_spec(validate_version).

validate_version(validate, _, _Id) ->
    %% Should validate against configured version?
    {ok, valid};

validate_version(fix, _, _) ->
    {error, invalid}.
