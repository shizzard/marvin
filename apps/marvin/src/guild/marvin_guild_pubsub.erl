-module(marvin_guild_pubsub).
-compile({parse_transform, cloak_transform}).

-export([
    type_command/0, type_channel_voice/0, type_voice_state/0, type_message/0,
    type_member/0, type_presence/0,
    type_internal/0,
    action_create/0, action_update/0, action_delete/0,
    action_internal_event_guild_provisioned/0, action_internal_event_guild_members_provisioned/0
]).
-export([channel/3, subscribe/3, publish/5]).

-record(?MODULE, {
    guild_context :: marvin_guild_context:t(),
    type :: binary(),
    action :: binary(),
    payload :: term()
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



%% Interface



type_command() -> <<"command">>.
type_channel_voice() -> <<"channel_voice">>.
type_voice_state() -> <<"voice_state">>.
type_message() -> <<"message">>.
type_member() -> <<"member">>.
type_presence() -> <<"presence">>.
type_internal() -> <<"internal">>.

action_create() -> <<"create">>.
action_update() -> <<"update">>.
action_delete() -> <<"delete">>.

action_internal_event_guild_provisioned() -> <<"guild_provisioned">>.
action_internal_event_guild_members_provisioned() -> <<"guild_members_provisioned">>.



channel(GuildId, Type, Action) ->
    <<"g:", GuildId/binary, "/t:", Type/binary, "/a:", Action/binary>>.



subscribe(GuildId, Type, Action) ->
    PubsubChannel = channel(GuildId, Type, Action),
    ebus:sub(self(), PubsubChannel).



publish(GuildId, GuildCtx, Type, Action, Payload) ->
    PubsubChannel = channel(GuildId, Type, Action),
    ebus:pub(PubsubChannel, new(#{
        guild_context => GuildCtx, type => Type, action => Action, payload => Payload
    })).
