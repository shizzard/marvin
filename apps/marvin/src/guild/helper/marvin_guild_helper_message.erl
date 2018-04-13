-module(marvin_guild_helper_message).

-include("marvin_guild_state.hrl").

-export([
    handle_call_message_create_chain/1
]).



%% Interface



-spec handle_call_message_create_chain({Struct :: marvin_pdu2_dispatch_message_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_message_create:t(),
        S1 :: state()
    }).

handle_call_message_create_chain({Struct, S0}) ->
    case marvin_pdu2_dispatch_message_create:content(Struct) of
        <<"channel">> ->
            Req = marvin_rest_request:new(
                marvin_rest_impl_guild_channel_create,
                #{<<"guild_id">> => S0#state.guild_id},
                #{
                    type => marvin_pdu2_rest_guild_channel_create:channel_type_guild_voice(),
                    name => list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                    bitrate => 64000,
                    parent_id => <<"434363244443074570">>
                }
            ),
            {ok, Resp} = marvin_rest_shotgun:request(Req),
            marvin_log:info("Guild '~s' handled guild channel create request: ~p", [S0#state.guild_id, Resp]);
        _ ->
            marvin_log:info("Guild '~s' handled message", [S0#state.guild_id]),
            ok
    end,
    {ok, {Struct, S0}}.
