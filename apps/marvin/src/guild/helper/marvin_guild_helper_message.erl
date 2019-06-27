-module(marvin_guild_helper_message).
-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([
    w_message_create/2
]).



-record(handle_possible_command, {
    original_message :: marvin_pdu2_dispatch_message_create:t(),
    tokenized_message_content :: [term()],
    parsed_message_content :: [term()],
    detected_command :: marvin_plugin_command:t() | undefined,
    ctx :: marvin_guild_context:t()
}).



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
            marvin_guild_pubsub:publish(
                marvin_guild_context:guild_id(Ctx),
                Ctx,
                marvin_guild_pubsub:type_message(),
                marvin_guild_pubsub:action_create(),
                Message
            ),
            ok
    end.



%% Internals



-spec handle_possible_command(
    Message :: marvin_pdu2_dispatch_message_create:t(),
    Ctx :: marvin_guild_context:t()
) ->
    marvin_helper_type:ok_return().

handle_possible_command(Message, Ctx) ->
    case marvin_helper_chain:chain('marvin_guild_helper_message:handle_possible_command', [
        fun handle_possible_command_run_pre_command_hooks/1,
        fun handle_possible_command_tokenize/1,
        fun handle_possible_command_parse/1,
        fun handle_possible_command_maybe_detect/1,
        fun handle_possible_command_run_command_or_response/1
    ], #handle_possible_command{original_message = Message, ctx = Ctx}) of
        {ok, _ChainCtx} ->
            ok;
        {stop, Reason} ->
            ?l_info(#{
                text => "Guild handle_possible_command stopped",
                what => handle_possible_command, result => stop,
                details => #{
                    error => Reason,
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    original_message => marvin_pdu2_dispatch_message_create:content(Message)
                }
            });
        {error, Reason} ->
            ?l_error(#{
                text => "Guild handle_possible_command failure",
                what => handle_possible_command, result => failure,
                details => #{
                    error => Reason,
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    original_message => marvin_pdu2_dispatch_message_create:content(Message)
                }
            })
    end.



handle_possible_command_run_pre_command_hooks(#handle_possible_command{
    original_message = Message,
    ctx = Ctx
} = ChainCtx) ->
    case run_pre_command_hooks(marvin_guild_pubsub:new(#{
        guild_context => Ctx,
        type => marvin_guild_pubsub:type_message(),
        action => marvin_guild_pubsub:action_create(),
        payload => Message
    }), marvin_guild_context:pre_command_hooks(Ctx)) of
        terminate ->
            ?l_info(#{
                text => "Guild command terminated",
                what => handle_possible_command, result => terminate,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    user_id => marvin_pdu2_object_user:id(
                        marvin_pdu2_dispatch_message_create:author(Message)),
                    user_username => marvin_pdu2_object_user:username(
                        marvin_pdu2_dispatch_message_create:author(Message))
                }
            }),
            {stop, terminate};
        skip ->
            {ok, ChainCtx}
    end.




handle_possible_command_tokenize(#handle_possible_command{
    original_message = Message
} = ChainCtx) ->
    case marvin_guild_command_lexer:string(
        unicode:characters_to_list(marvin_pdu2_dispatch_message_create:content(Message))
    ) of
        {ok, Tokens, _} ->
            {ok, ChainCtx#handle_possible_command{tokenized_message_content = Tokens}};
        {error, Reason} ->
            {error, Reason}
    end.



handle_possible_command_parse(#handle_possible_command{
    tokenized_message_content = Tokens
} = ChainCtx) ->
    case marvin_guild_command_parser:parse(Tokens) of
        {ok, ParsedMessage} ->
            {ok, ChainCtx#handle_possible_command{
                parsed_message_content = ParsedMessage
            }};
        {error, Reason} ->
            {error, Reason}
    end.



handle_possible_command_maybe_detect(#handle_possible_command{
    original_message = OriginalMessage,
    parsed_message_content = ParsedMessage,
    ctx = Ctx
} = ChainCtx) ->
    Words = [string:lowercase(Part) || Part <- ParsedMessage, not is_integer(Part), not is_tuple(Part)],
    Commands = marvin_guild_context:commands(Ctx),
    case lists:foldl(fun
        (FoldCommand, undefined) ->
            case lists:any(fun(Keyword) -> lists:member(Keyword, Words) end, marvin_plugin_command:keywords(FoldCommand)) of
                true ->
                    FoldCommand;
                false ->
                    undefined
            end;
        (_Command, Ret) ->
            Ret
    end, undefined, Commands) of
        undefined ->
            {ok, ChainCtx};
        Command ->
            ?l_notice(#{
                text => "Guild handle_possible_command command detected",
                what => handle_possible_command, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    plugin => marvin_plugin_command:plugin_id(Command),
                    command => marvin_plugin_command:command(Command),
                    user_id => marvin_pdu2_object_user:id(
                        marvin_pdu2_dispatch_message_create:author(OriginalMessage)),
                    user_username => marvin_pdu2_object_user:username(
                        marvin_pdu2_dispatch_message_create:author(OriginalMessage))
                }
            }),
            {ok, ChainCtx#handle_possible_command{detected_command = Command}}
    end.



handle_possible_command_run_command_or_response(#handle_possible_command{
    detected_command = undefined,
    original_message = Message,
    ctx = Ctx
} = ChainCtx) ->
    case marvin_dialogflow:detect_intent(
        marvin_guild_context:guild_id(Ctx),
        marvin_pdu2_object_user:id(marvin_pdu2_dispatch_message_create:author(Message)),
        marvin_pdu2_dispatch_message_create:content(Message)
    ) of
        {ok, DialogFlowResponse} ->
            SendReq = marvin_rest2_request:new(
                marvin_rest2_impl_message_create,
                #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(Message)},
                #{content => marvin_dialogflow_response_result:fulfillment(marvin_dialogflow_response:result(DialogFlowResponse))}
            ),
            marvin_rest2:enqueue_request(SendReq),
            {ok, ChainCtx};
        {error, Reason} ->
            {error, Reason}
    end;

handle_possible_command_run_command_or_response(#handle_possible_command{
    detected_command = Command,
    original_message = Message,
    parsed_message_content = ParsedMessage,
    ctx = Ctx
} = ChainCtx) ->
    marvin_guild_pubsub:publish(
        marvin_guild_context:guild_id(Ctx),
        Ctx,
        marvin_guild_pubsub:type_command(),
        marvin_plugin_command:short(Command),
        #{
            command => marvin_plugin_command:command(Command),
            original_message => Message,
            parsed_message_content => ParsedMessage
        }
    ),
    {ok, ChainCtx}.



run_pre_command_hooks(Event, PreCommandHooks) ->
    Message = marvin_guild_pubsub:payload(Event),
    Ctx = marvin_guild_pubsub:guild_context(Event),
    maps:fold(fun(PluginId, Fun, Acc) ->
        ?l_info(#{
            text => "Running pre-command hook",
            what => handle_possible_command, result => skip,
            details => #{
                plugin_id => PluginId,
                guild_id => marvin_guild_context:guild_id(Ctx),
                user_id => marvin_pdu2_object_user:id(
                    marvin_pdu2_dispatch_message_create:author(Message)),
                user_username => marvin_pdu2_object_user:username(
                    marvin_pdu2_dispatch_message_create:author(Message))
            }
        }),
        case Fun(Event) of
            skip when terminate == Acc -> terminate;
            skip -> skip;
            terminate ->
                ?l_info(#{
                    text => "Plugin terminated the command execution",
                    what => handle_possible_command, result => skip,
                    details => #{
                        plugin_id => PluginId,
                        guild_id => marvin_guild_context:guild_id(Ctx),
                        user_id => marvin_pdu2_object_user:id(
                            marvin_pdu2_dispatch_message_create:author(Message)),
                        user_username => marvin_pdu2_object_user:username(
                            marvin_pdu2_dispatch_message_create:author(Message))
                    }
                }),
                terminate
        end
    end, skip, PreCommandHooks).
