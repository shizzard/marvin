-module(marvin_guild_helper_message).

-include("marvin_guild_state.hrl").

-export([
    w_message_create/2
]).



%% Interface



-spec w_message_create(Message :: marvin_pdu2_dispatch_message_create:t(), Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_message_create(Message, Ctx) ->
    case marvin_pdu2_dispatch_message_create:content(Message) of
        <<"channel">> ->
            Req = marvin_rest_request:new(
                marvin_rest_impl_guild_channel_create,
                #{<<"guild_id">> => marvin_guild_context:guild_id(Ctx)},
                #{
                    type => marvin_pdu2_rest_guild_channel_create:channel_type_guild_voice(),
                    name => list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                    bitrate => 64000,
                    parent_id => <<"434363244443074570">>
                }
            ),
            {ok, Resp} = marvin_rest_shotgun:request(Req),
            marvin_log:info("Guild '~s' handled guild channel create request: ~p", [marvin_guild_context:guild_id(Ctx), Resp]),
            ok;
        _ ->
            marvin_log:info("Guild '~s' handled message", [marvin_guild_context:guild_id(Ctx)]),
            ok
    end.
