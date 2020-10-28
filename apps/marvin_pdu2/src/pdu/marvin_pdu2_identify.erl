-module(marvin_pdu2_identify).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
-export([
    create_intent/1,
    intent_guilds/0, intent_guild_members/0, intent_guild_bans/0,
    intent_guild_emojis/0, intent_guild_integrations/0, intent_guild_webhooks/0,
    intent_guild_invites/0, intent_guild_voice_states/0, intent_guild_presences/0,
    intent_guild_messages/0, intent_guild_message_reactions/0,
    intent_guild_message_typing/0, intent_direct_messages/0,
    intent_direct_message_reactions/0, intent_direct_message_typing/0
]).

-define(INTENT_GUILDS, 16#00000001).
-define(INTENT_GUILD_MEMBERS, 16#00000002).
-define(INTENT_GUILD_BANS, 16#00000004).
-define(INTENT_GUILD_EMOJIS, 16#00000008).
-define(INTENT_GUILD_INTEGRATIONS, 16#00000010).
-define(INTENT_GUILD_WEBHOOKS, 16#00000020).
-define(INTENT_GUILD_INVITES, 16#00000040).
-define(INTENT_GUILD_VOICE_STATES, 16#00000080).
-define(INTENT_GUILD_PRESENCES, 16#00000100).
-define(INTENT_GUILD_MESSAGES, 16#00000200).
-define(INTENT_GUILD_MESSAGE_REACTIONS, 16#00000400).
-define(INTENT_GUILD_MESSAGE_TYPING, 16#00000800).
-define(INTENT_DIRECT_MESSAGES, 16#00001000).
-define(INTENT_DIRECT_MESSAGE_REACTIONS, 16#00002000).
-define(INTENT_DIRECT_MESSAGE_TYPING, 16#00004000).

-record(?MODULE, {
    token = undefined :: token(),
    compress = undefined :: compress(),
    large_threshold = undefined :: large_threshold(),
    properties = undefined :: properties(),
    shard = undefined :: shard(),
    prot_shard = undefined :: prot_shard(),
    prot_total_shards = undefined :: prot_total_shards(),
    intents = undefined :: intents()
}).

-type token() :: marvin_pdu2:token().
-type compress() :: boolean().
-type large_threshold() :: 50..250.
-type properties() :: marvin_pdu2_identify_properties:t().
-type shard() :: marvin_pdu2:shard_spec().
-type prot_shard() :: marvin_pdu2:shard().
-type prot_total_shards() :: marvin_pdu2:total_shards().
-type intent() :: pos_integer().
-type intents() :: pos_integer().
-type t() :: #?MODULE{}.

-export_type([
    token/0, compress/0, large_threshold/0, properties/0, shard/0,
    prot_shard/0, prot_total_shards/0, intent/0, intents/0, t/0
]).

cloak_validate(properties, Value) ->
    {ok, marvin_pdu2_identify_properties:new(Value)};

cloak_validate(shard, [Shard, TotalShards] = Value)
when is_integer(Shard) andalso is_integer(TotalShards)
andalso Shard < TotalShards ->
    {ok, Value};

cloak_validate(_, Value) ->
    {ok, Value}.


cloak_validate_struct(#?MODULE{shard = [Shard, TotalShards]} = Struct) ->
    {ok, Struct#?MODULE{prot_shard = Shard, prot_total_shards = TotalShards}};

cloak_validate_struct(_Struct) ->
    {error, invalid}.


export(#?MODULE{
    token = Token,
    compress = Compress,
    large_threshold = LargeThreshold,
    properties = Properties,
    shard = ShardSpec,
    intents = Intents
}) ->
    #{
        <<"token">> => marvin_pdu2:nullify(Token),
        <<"compress">> => marvin_pdu2:nullify(Compress),
        <<"large_threshold">> => marvin_pdu2:nullify(LargeThreshold),
        <<"properties">> => marvin_pdu2_identify_properties:export(Properties),
        <<"shard">> => marvin_pdu2:nullify(ShardSpec),
        <<"intents">> => marvin_pdu2:nullify(Intents)
    }.


-spec create_intent(List :: [intent(), ...]) ->
    Ret :: intents().

create_intent(IntentList) ->
    lists:foldl(fun(Intent, Acc) ->
        Intent bor Acc
    end, 0, IntentList).


intent_guilds() -> ?INTENT_GUILDS.
intent_guild_members() -> ?INTENT_GUILD_MEMBERS.
intent_guild_bans() -> ?INTENT_GUILD_BANS.
intent_guild_emojis() -> ?INTENT_GUILD_EMOJIS.
intent_guild_integrations() -> ?INTENT_GUILD_INTEGRATIONS.
intent_guild_webhooks() -> ?INTENT_GUILD_WEBHOOKS.
intent_guild_invites() -> ?INTENT_GUILD_INVITES.
intent_guild_voice_states() -> ?INTENT_GUILD_VOICE_STATES.
intent_guild_presences() -> ?INTENT_GUILD_PRESENCES.
intent_guild_messages() -> ?INTENT_GUILD_MESSAGES.
intent_guild_message_reactions() -> ?INTENT_GUILD_MESSAGE_REACTIONS.
intent_guild_message_typing() -> ?INTENT_GUILD_MESSAGE_TYPING.
intent_direct_messages() -> ?INTENT_DIRECT_MESSAGES.
intent_direct_message_reactions() -> ?INTENT_DIRECT_MESSAGE_REACTIONS.
intent_direct_message_typing() -> ?INTENT_DIRECT_MESSAGE_TYPING.

