-module(marvin_guild_helper_voice_state).

-include("marvin_guild_state.hrl").

-export([
    w_voice_state_update/2,
    r_get_participant_channel/2,
    r_get_channel_participants/2
]).



%% Interface



-spec w_voice_state_update(VoiceStateUpdate :: marvin_pdu2_dispatch_voice_state_update:t(), Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_voice_state_update(VoiceStateUpdate, Ctx) ->
    UserId = marvin_pdu2_dispatch_voice_state_update:user_id(VoiceStateUpdate),
    ChannelId = marvin_pdu2_dispatch_voice_state_update:channel_id(VoiceStateUpdate),
    OldChannelId = r_get_participant_channel(UserId, Ctx),
    case {OldChannelId, ChannelId} of
        {undefined, ChannelId} ->
            marvin_log:debug(
                "Guild '~s' voice channel: user '~s' joined to channel '~s'",
                [marvin_guild_context:guild_id(Ctx), UserId, ChannelId]
            ),
            ets:insert(marvin_guild_context:voice_state(Ctx), #voice_state{user_id = UserId, channel_id = ChannelId});
        {OldChannelId, undefined} ->
            marvin_log:debug(
                "Guild '~s' voice channel: user '~s' left channel '~s'",
                [marvin_guild_context:guild_id(Ctx), UserId, OldChannelId]
            ),
            ets:delete(marvin_guild_context:voice_state(Ctx), UserId);
        {OldChannelId, ChannelId} ->
            marvin_log:debug(
                "Guild '~s' voice channel: user '~s' moved from channel '~s' to channel '~s'",
                [marvin_guild_context:guild_id(Ctx), UserId, OldChannelId, ChannelId]
            ),
            ets:insert(marvin_guild_context:voice_state(Ctx), #voice_state{user_id = UserId, channel_id = ChannelId})
    end,
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
