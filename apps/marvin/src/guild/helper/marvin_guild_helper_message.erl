-module(marvin_guild_helper_message).

-include("marvin_guild_state.hrl").

-export([
    w_message_create/2
]).

-define(debug_hotword, <<"%debug%">>).
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
            maybe_run_command(Message, ParsedMessage, Ctx),
            ok;
        {error, Reason} ->
            marvin_log:error(
                "Guild '~s' failed to parse possible command with reason '~p'",
                [marvin_guild_context:guild_id(Ctx), Reason]
            )
    end.



handle_possible_command_tokenize(Binary) ->
    case marvin_guild_command_lexer:string(string:lowercase(unicode:characters_to_list(Binary))) of
        {ok, Tokens, _} -> {ok, Tokens};
        {error, Reason, _} -> {error, Reason}
    end.



handle_possible_command_parse(Tokens) ->
    marvin_guild_command_parser:parse(Tokens).



maybe_run_command(OriginalMessage, ParsedMessage, Ctx) ->
    Words = [Part || Part <- ParsedMessage, not is_integer(Part), not is_tuple(Part)],
    case Words of
        [] ->
            ok;
        _ ->
            Commands = marvin_guild_context:commands(Ctx),
            {Scores, _} = lists:foldl(fun maybe_run_command_calculate_score/2, {[], Words}, Commands),
            ok = maybe_send_debug_info(OriginalMessage, Words, Scores),
            %% reverse sort here
            case lists:sort(fun({_, Score1}, {_, Score2}) -> Score1 >= Score2 end, Scores) of
                [{_WinningCommand, Score} | _] when Score =< ?lower_score_mark ->
                    marvin_log:info("Guild '~s' no winning command", [marvin_guild_context:guild_id(Ctx)]),
                    false;
                [{WinningCommand, Score} | _] ->
                    marvin_log:info("Guild '~s' winning command: ~ts/~ts: ~.3f", [
                        marvin_guild_context:guild_id(Ctx),
                        marvin_plugin_command:plugin_id(WinningCommand),
                        marvin_plugin_command:command(WinningCommand),
                        Score
                    ]),
                    marvin_guild_pubsub:publish(
                        marvin_guild_context:guild_id(Ctx),
                        Ctx,
                        marvin_guild_pubsub:type_command(),
                        marvin_plugin_command:short(WinningCommand),
                        #{
                            command => marvin_plugin_command:command(WinningCommand),
                            original_message => OriginalMessage,
                            parsed_message => ParsedMessage
                        }
                    ),
                    true
            end
    end.



maybe_run_command_calculate_score(Command, {Acc, Words}) ->
    Keywords = marvin_plugin_command:keywords(Command),
    Score = length(lists:filter(fun(Keyword) ->
        lists:member(Keyword, Words)
    end, Keywords)) / length(Words),
    {[{Command, Score} | Acc], Words}.



maybe_send_debug_info(OriginalMessage, Words, Scores) ->
    case lists:member(?debug_hotword, Words) of
        true ->
            SendReq = marvin_rest_request:new(
                marvin_rest_impl_message_create,
                #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
                #{
                    content => <<"`", (marvin_pdu2_dispatch_message_create:content(OriginalMessage))/binary, "`">>,
                    embed => #{
                        title => <<"Debug info requested">>,
                        type => <<"rich">>,
                        fields => lists:map(fun maybe_send_debug_info_format_score_as_field/1, Scores)
                    }
                }
            ),
            marvin_rest_shotgun:request(SendReq),
            ok;
        false ->
            ok
    end.



maybe_send_debug_info_format_score_as_field({Command, Score}) ->
    #{
        name => marvin_plugin_command:short(Command),
        value => iolist_to_binary(io_lib:format("~.3f", [Score])),
        inline => false
    }.
