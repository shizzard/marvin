-module(marvin_guild_helper_voice_state).
-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([
    w_do_provision/2,
    w_voice_state_update/2,
    r_get_participant_channel/2,
    r_get_channel_participants/2
]).



%% Interface



-spec w_do_provision(VoiceStates :: [marvin_pdu2_object_voice_state:t()], Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_do_provision(VoiceStates, Ctx) ->
    ?l_debug(#{
        text => "Guild voice states provisioned",
        what => voice_state_provision, result => ok,
        details => #{
            guild_id => marvin_guild_context:guild_id(Ctx),
            total => length(VoiceStates)
        }
    }),
    [
        ets:insert(marvin_guild_context:voice_state(Ctx), #voice_state{
            user_id = marvin_pdu2_object_voice_state:user_id(VoiceState),
            channel_id = marvin_pdu2_object_voice_state:channel_id(VoiceState)
        }) || VoiceState <- VoiceStates
    ],
    ok.



-spec w_voice_state_update(VoiceStateUpdate :: marvin_pdu2_dispatch_voice_state_update:t(), Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_voice_state_update(VoiceStateUpdate, Ctx) ->
    UserId = marvin_pdu2_dispatch_voice_state_update:user_id(VoiceStateUpdate),
    ChannelId = marvin_pdu2_dispatch_voice_state_update:channel_id(VoiceStateUpdate),
    OldChannelId = r_get_participant_channel(UserId, Ctx),
    case {OldChannelId, ChannelId} of
        {undefined, ChannelId} ->
            ?l_debug(#{
                text => "Guild voice state update",
                what => voice_state_update, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    user_id => UserId,
                    channel_id => ChannelId,
                    kind => join
                }
            }),
            ets:insert(marvin_guild_context:voice_state(Ctx), #voice_state{user_id = UserId, channel_id = ChannelId});
        {OldChannelId, undefined} ->
            ?l_debug(#{
                text => "Guild voice state update",
                what => voice_state_update, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    user_id => UserId,
                    channel_id => OldChannelId,
                    kind => leave
                }
            }),
            ets:delete(marvin_guild_context:voice_state(Ctx), UserId);
        {OldChannelId, ChannelId} ->
            ?l_debug(#{
                text => "Guild voice state update",
                what => voice_state_update, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    user_id => UserId,
                    from_channel_id => OldChannelId,
                    channel_id => ChannelId,
                    kind => move
                }
            }),
            ets:insert(marvin_guild_context:voice_state(Ctx), #voice_state{user_id = UserId, channel_id = ChannelId})
    end,
    marvin_guild_pubsub:publish(
        marvin_guild_context:guild_id(Ctx),
        Ctx,
        marvin_guild_pubsub:type_voice_state(),
        marvin_guild_pubsub:action_update(),
        VoiceStateUpdate
    ),
    ok.



-spec r_get_participant_channel(UserId :: marvin_pdu2:snowflake(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_pdu2_object_voice_state:status().

r_get_participant_channel(UserId, Ctx) ->
    case ets:lookup(marvin_guild_context:voice_state(Ctx), UserId) of
        [] ->
            undefined;
        [#voice_state{user_id = UserId, channel_id = ChannelId}] ->
            ChannelId
    end.



-spec r_get_channel_participants(ChannelId :: marvin_pdu2:snowflake(), Ctx :: marvin_guild_context:t()) ->
    Ret :: [marvin_pdu2:snowflake()].

r_get_channel_participants(ChannelId, Ctx) ->
    [
        VoiceState#voice_state.user_id
        || VoiceState <- ets:tab2list(marvin_guild_context:voice_state(Ctx)),
        ChannelId == VoiceState#voice_state.channel_id
    ].



%% Internals
