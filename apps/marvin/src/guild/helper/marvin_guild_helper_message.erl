-module(marvin_guild_helper_message).

-include("marvin_guild_state.hrl").

-export([
    w_message_create/2
]).

-define(lower_score_mark, 0.15).
-define(debug_hotword, <<"%debug%">>).



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
        fun handle_possible_command_tokenize/1,
        fun handle_possible_command_parse/1,
        fun handle_possible_command_maybe_detect/1,
        fun handle_possible_command_run_command_or_response/1
    ], #handle_possible_command{original_message = Message, ctx = Ctx}) of
        {ok, _ChainCtx} ->
            ok;
        {error, Reason} ->
            marvin_log:error(
                "Guild '~s' failed to parse possible command with reason '~p'",
                [marvin_guild_context:guild_id(Ctx), Reason]
            )
    end.



handle_possible_command_tokenize(#handle_possible_command{
    original_message = Message
} = ChainCtx) ->
    case marvin_guild_command_lexer:string(
        unicode:characters_to_list(marvin_pdu2_dispatch_message_create:content(Message))
    ) of
        {ok, Tokens, _} ->
            marvin_log:info("Tokenized message: ~p", [Tokens]),
            {ok, ChainCtx#handle_possible_command{tokenized_message_content = Tokens}};
        {error, Reason} ->
            {error, Reason}
    end.



handle_possible_command_parse(#handle_possible_command{
    tokenized_message_content = Tokens
} = ChainCtx) ->
    case marvin_guild_command_parser:parse(Tokens) of
        {ok, ParsedMessage} ->
            marvin_log:info("Parsed message: ~p", [ParsedMessage]),
            {ok, ChainCtx#handle_possible_command{
                parsed_message_content = ParsedMessage
            }};
        {error, Reason} ->
            {error, Reason}
    end.



handle_possible_command_maybe_detect(#handle_possible_command{
    parsed_message_content = ParsedMessage,
    ctx = Ctx
} = ChainCtx) ->
    Words = [Part || Part <- ParsedMessage, not is_integer(Part), not is_tuple(Part)],
    marvin_log:info("~p", [Words]),
    Commands = marvin_guild_context:commands(Ctx),
    case lists:foldl(fun
        (Command, undefined) ->
            case lists:any(fun(Keyword) -> lists:member(Keyword, Words) end, marvin_plugin_command:keywords(Command)) of
                true -> Command;
                false -> undefined
            end;
        (_Command, Ret) ->
            Ret
    end, undefined, Commands) of
        undefined ->
            {ok, ChainCtx};
        Command ->
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
            SendReq = marvin_rest_request:new(
                marvin_rest_impl_message_create,
                #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(Message)},
                #{content => marvin_dialogflow_response_result:fulfillment(marvin_dialogflow_response:result(DialogFlowResponse))}
            ),
            marvin_rest:request(SendReq),
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
