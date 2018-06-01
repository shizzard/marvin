-module(marvin_guild_helper_message).

-include("marvin_guild_state.hrl").

-export([
    w_message_create/2
]).

-define(lower_score_mark, 0.15).



%% Interface



-spec w_message_create(
    Message :: marvin_pdu2_dispatch_message_create:t(),
    Ctx :: marvin_guild_context:t()
) ->
    marvin_helper_type:ok_return().

w_message_create(Message, Ctx) ->
    MyId = marvin_guild_context:my_id(Ctx),
    IsUserMessage = not marvin_pdu2_object_user:bot(marvin_pdu2_dispatch_message_create:author(Message)),
    IsMentioned = lists:any(fun(User) ->
        marvin_pdu2_object_user:is(User, MyId)
    end, marvin_pdu2_dispatch_message_create:mentions(Message)),
    case {IsUserMessage, IsMentioned} of
        {true, true} ->
            erlang:spawn(fun() -> handle_possible_command(Message, Ctx) end),
            ok;
        {_, _} ->
            ok
    end.



%% Internals



-spec handle_possible_command(
    Message :: marvin_pdu2_dispatch_message_create:t(),
    Ctx :: marvin_guild_context:t()
) ->
    marvin_helper_type:ok_return().

handle_possible_command(Message, Ctx) ->
    case marvin_dialogflow:detect_intent(
        marvin_guild_context:guild_id(Ctx),
        marvin_pdu2_object_user:id(marvin_pdu2_dispatch_message_create:author(Message)),
        marvin_pdu2_dispatch_message_create:content(Message)
    ) of
        {ok, DialogFlowResponse} ->
            maybe_run_command(Message, DialogFlowResponse, Ctx),
            ok;
        {error, Reason} ->
            marvin_log:error(
                "Guild '~s' failed to parse possible command with reason '~p'",
                [marvin_guild_context:guild_id(Ctx), Reason]
            )
    end.



maybe_run_command(OriginalMessage, DialogFlowResponse, Ctx) ->
    case binary:split(
        marvin_dialogflow_response_result:action(marvin_dialogflow_response:result(DialogFlowResponse)),
        <<".">>, [global]
    ) of
        [<<"marvin">>, PluginId, CommandId] ->
            marvin_log:info(
                "Guild '~s' winning command according to DialogFlow response: ~ts/~ts with score ~.3f",
                [marvin_guild_context:guild_id(Ctx), PluginId, CommandId,
                marvin_dialogflow_response_result:score(marvin_dialogflow_response:result(DialogFlowResponse))]
            ),
            case find_command(PluginId, CommandId, marvin_guild_context:commands(Ctx)) of
                undefined ->
                    marvin_log:info(
                        "Guild '~s' is passing ~ts/~ts command to fallback as-is handler",
                        [marvin_guild_context:guild_id(Ctx), PluginId, CommandId]
                    );
                Command ->
                    marvin_guild_pubsub:publish(
                        marvin_guild_context:guild_id(Ctx),
                        Ctx,
                        marvin_guild_pubsub:type_command(),
                        marvin_plugin_command:short(Command),
                        #{
                            command => marvin_plugin_command:command(Command),
                            original_message => OriginalMessage,
                            dialogflow_response => DialogFlowResponse
                        }
                    )
            end;
        _Other ->
            send_dialogflow_response(OriginalMessage, DialogFlowResponse)
    end.



find_command(_PluginId, _CommandId, []) ->
    undefined;

find_command(PluginId, CommandId, [Command | Commands]) ->
    case {marvin_plugin_command:plugin_id(Command), marvin_plugin_command:command(Command)} of
        {PluginId, CommandId} ->
            Command;
        {_, _} ->
            find_command(PluginId, CommandId, Commands)
    end.



send_dialogflow_response(OriginalMessage, DialogFlowResponse) ->
    SendReq = marvin_rest_request:new(
        marvin_rest_impl_message_create,
        #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
        #{content => marvin_dialogflow_response_result:fulfillment(marvin_dialogflow_response:result(DialogFlowResponse))}
    ),
    marvin_rest:request(SendReq),
    ok.
