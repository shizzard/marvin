-module(marvin_guild_helper_channel_text).

-include("marvin_guild_state.hrl").

-export([handle_call_do_provision_chain/1]).



%% Interface



-spec handle_call_do_provision_chain({Struct :: marvin_pdu2_dispatch_guild_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_guild_create:t(),
        S1 :: state()
    }).

handle_call_do_provision_chain({Struct, S0}) ->
    Channels = [
        Channel || Channel <- marvin_pdu2_dispatch_guild_create:channels(Struct),
        marvin_pdu2_object_channel:channel_type_guild_text() == marvin_pdu2_object_channel:type(Channel)
    ],
    marvin_log:info("Guild '~s' text channels: ~p total", [S0#state.guild_id, length(Channels)]),
    S1 = lists:foldl(fun set_channel_text/2, S0, Channels),
    {ok, {Struct, S1}}.



%% Internals



-spec set_channel_text(ChannelStruct :: marvin_pdu2_object_channel:t(), S0 :: state()) ->
    Ret :: state().

set_channel_text(ChannelStruct, #state{channel_text_state = ChannelStateEts} = S0) ->
    ets:insert(ChannelStateEts, #channel{
        channel_id = marvin_pdu2_object_channel:id(ChannelStruct),
        channel = ChannelStruct
    }),
    S0.
