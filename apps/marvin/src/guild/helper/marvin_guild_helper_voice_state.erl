-module(marvin_guild_helper_voice_state).

-include("marvin_guild_state.hrl").

-export([
    handle_call_voice_state_update_chain/1,
    get_participant_channel/2,
    get_channel_participants/2
]).



%% Interface



-spec handle_call_voice_state_update_chain({Struct :: marvin_pdu2_dispatch_voice_state_update:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_voice_state_update:t(),
        S1 :: state()
    }).

handle_call_voice_state_update_chain({Struct, #state{
    voice_state = PresenceStateEts
} = S0}) ->
    UserId = marvin_pdu2_dispatch_voice_state_update:user_id(Struct),
    ChannelId = marvin_pdu2_dispatch_voice_state_update:channel_id(Struct),
    OldChannelId = get_participant_channel(UserId, S0),
    case {OldChannelId, ChannelId} of
        {undefined, ChannelId} ->
            marvin_log:info(
                "Guild '~s' voice channel: user '~s' joined to channel '~s'",
                [S0#state.guild_id, UserId, ChannelId]
            ),
            ets:insert(PresenceStateEts, #voice_state{user_id = UserId, channel_id = ChannelId});
        {OldChannelId, undefined} ->
            marvin_log:info(
                "Guild '~s' voice channel: user '~s' left channel '~s'",
                [S0#state.guild_id, UserId, OldChannelId]
            ),
            ets:delete(PresenceStateEts, UserId);
        {OldChannelId, ChannelId} ->
            marvin_log:info(
                "Guild '~s' voice channel: user '~s' moved from channel '~s' to channel '~s'",
                [S0#state.guild_id, UserId, OldChannelId, ChannelId]
            ),
            ets:insert(PresenceStateEts, #voice_state{user_id = UserId, channel_id = ChannelId})
    end,
    {ok, {Struct, S0}}.



-spec get_participant_channel(UserId :: marvin_pdu2:snowflake(), S0 :: state()) ->
    Ret :: marvin_pdu2_object_voice_state:status().

get_participant_channel(UserId, #state{
    voice_state = PresenceStateEts
}) ->
    case ets:lookup(PresenceStateEts, UserId) of
        [] ->
            undefined;
        [#voice_state{user_id = UserId, channel_id = ChannelId}] ->
            ChannelId
    end.



-spec get_channel_participants(ChannelId :: marvin_pdu2:snowflake(), S0 :: state()) ->
    Ret :: [marvin_pdu2:snowflake()].

get_channel_participants(ChannelId, #state{
    voice_state = PresenceStateEts
}) ->
    [
        VoiceState#voice_state.user_id
        || VoiceState <- ets:tab2list(PresenceStateEts),
        ChannelId == VoiceState#voice_state.channel_id
    ].



%% Internals
