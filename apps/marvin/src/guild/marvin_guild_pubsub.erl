-module(marvin_guild_pubsub).
-compile({parse_transform, cloak_transform}).

-export([channel/3, subscribe/3, publish/4]).

-record(?MODULE, {
    guild_id :: marvin_pdu2:snowflake(),
    type :: binary(),
    action :: binary(),
    payload :: term()
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



%% Interface



channel(GuildId, Type, Action) ->
    <<"g:", GuildId/binary, "/t:", Type/binary, "/a:", Action/binary>>.



subscribe(GuildId, Type, Action) ->
    PubsubChannel = channel(GuildId, Type, Action),
    ebus:sub(self(), PubsubChannel).



publish(GuildId, Type, Action, Payload) ->
    PubsubChannel = channel(GuildId, Type, Action),
    ebus:pub(PubsubChannel, new(#{
        guild_id => GuildId, type => Type, action => Action, payload => Payload
    })).
