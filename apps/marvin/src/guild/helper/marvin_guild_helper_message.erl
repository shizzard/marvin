-module(marvin_guild_helper_message).

-include("marvin_guild_state.hrl").

-export([
    w_message_create/2
]).



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
            handle_possible_command(Message, Ctx);
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
    case marvin_helper_chain:chain('marvin_guild_helper_message:handle_possible_command', [
        fun handle_possible_command_tokenize/1,
        fun handle_possible_command_parse/1
    ], marvin_pdu2_dispatch_message_create:content(Message)) of
        {ok, ParsedMessage} ->
            detect_command(ParsedMessage, Ctx);
        {error, Reason} ->
            marvin_log:error(
                "Guild '~s' failed to parse possible command with reason '~p'",
                [marvin_guild_context:guild_id(Ctx), Reason]
            )
    end.



handle_possible_command_tokenize(Binary) ->
    case marvin_guild_command_lexer:string(binary_to_list(Binary)) of
        {ok, Tokens, _} -> {ok, Tokens};
        {error, Reason} -> {error, Reason}
    end.



handle_possible_command_parse(Tokens) ->
    marvin_guild_command_parser:parse(Tokens).



detect_command(ParsedMessage, _Ctx) ->
    marvin_log:info("~p", [ParsedMessage]).
