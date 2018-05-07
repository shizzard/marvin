-module(marvin_guild_helper_channel_voice).

-include("marvin_guild_state.hrl").

-export([
    w_do_provision/2, w_channel_create/2,
    w_channel_update/2, w_channel_delete/2,
    r_get_channels_by_category/2
]).



%% Interface



-spec w_do_provision(Channels :: [marvin_pdu2_object_channel:t()], Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_do_provision(Channels, Ctx) ->
    VoiceChannels = [
        Channel || Channel <- Channels,
        marvin_pdu2_object_channel:channel_type_guild_voice() == marvin_pdu2_object_channel:type(Channel)
    ],
    marvin_log:info("Guild '~s' voice channels: ~p total", [marvin_guild_context:guild_id(Ctx), length(VoiceChannels)]),
    lists:foreach(fun(Channel) -> set_channel_voice(Channel, Ctx) end, VoiceChannels),
    ok.



-spec w_channel_create(ChannelEvent :: marvin_pdu2_dispatch_channel_create:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_create(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_create:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_create:channel_type_guild_voice()
    } of
        {_Same, _Same} ->
            marvin_log:info(
                "Guild '~s' got new voice channel '~s'",
                [marvin_guild_context:guild_id(Ctx), marvin_pdu2_dispatch_channel_create:id(ChannelEvent)
            ]),
            marvin_guild_pubsub:publish(
                marvin_guild_context:guild_id(Ctx),
                Ctx,
                marvin_guild_pubsub:type_channel_voice(),
                marvin_guild_pubsub:action_create(),
                ChannelEvent
            ),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_create:export(ChannelEvent)),
            set_channel_voice(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec w_channel_update(ChannelEvent :: marvin_pdu2_dispatch_channel_update:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_update(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_update:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_update:channel_type_guild_voice()
    } of
        {_Same, _Same} ->
            marvin_log:info(
                "Guild '~s' got updated voice channel '~s'",
                [marvin_guild_context:guild_id(Ctx), marvin_pdu2_dispatch_channel_update:id(ChannelEvent)
            ]),
            marvin_guild_pubsub:publish(
                marvin_guild_context:guild_id(Ctx),
                Ctx,
                marvin_guild_pubsub:type_channel_voice(),
                marvin_guild_pubsub:action_update(),
                ChannelEvent
            ),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_update:export(ChannelEvent)),
            set_channel_voice(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec w_channel_delete(ChannelEvent :: marvin_pdu2_dispatch_channel_delete:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_delete(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_delete:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_delete:channel_type_guild_voice()
    } of
        {_Same, _Same} ->
            marvin_log:info(
                "Guild '~s' lost voice channel '~s'",
                [marvin_guild_context:guild_id(Ctx), marvin_pdu2_dispatch_channel_delete:id(ChannelEvent)
            ]),
            marvin_guild_pubsub:publish(
                marvin_guild_context:guild_id(Ctx),
                Ctx,
                marvin_guild_pubsub:type_channel_voice(),
                marvin_guild_pubsub:action_delete(),
                ChannelEvent
            ),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_delete:export(ChannelEvent)),
            delete_channel_voice(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec r_get_channels_by_category(CategoryId :: marvin_pdu2:snowflake(), Ctx :: marvin_guild_context:t()) ->
    Ret :: [marvin_pdu2_object_channel:t()].

r_get_channels_by_category(CategoryId, Ctx) ->
    [
        Channel#channel.channel ||
        Channel <- ets:tab2list(marvin_guild_context:channel_voice_state(Ctx)),
        CategoryId == marvin_pdu2_object_channel:parent_id(Channel#channel.channel)
    ].



%% Internals



-spec set_channel_voice(Channel :: marvin_pdu2_object_channel:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

set_channel_voice(Channel, Ctx) ->
    ChannelId = marvin_pdu2_object_channel:id(Channel),
    marvin_channel_registry:register(marvin_guild_context:guild_id(Ctx), ChannelId),
    ets:insert(marvin_guild_context:channel_voice_state(Ctx), #channel{channel_id = ChannelId, channel = Channel}),
    ok.



-spec delete_channel_voice(Channel :: marvin_pdu2_object_channel:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

delete_channel_voice(Channel, Ctx) ->
    ChannelId = marvin_pdu2_object_channel:id(Channel),
    marvin_channel_registry:register(marvin_guild_context:guild_id(Ctx), ChannelId),
    ets:delete(marvin_guild_context:channel_voice_state(Ctx), ChannelId),
    ok.
