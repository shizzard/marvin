-module(marvin_plugin_random).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(guess_answers(), [
    %% strict yes
    <<"Да."/utf8>>,
    <<"Несомненно, да."/utf8>>,
    <<"Это бесспорно, да."/utf8>>,
    <<"Можешь быть в этом уверен."/utf8>>,
    %% maybe yes
    <<"Скорее всего да."/utf8>>,
    <<"Предположительно да."/utf8>>,
    <<"Мне кажется, что да."/utf8>>,
    <<"Это очень вероятно."/utf8>>,
    %% neutral
    <<"Сейчас мне лень отвечать."/utf8>>,
    <<"Это сложный вопрос, я не готов на него ответить."/utf8>>,
    <<"Может быть, я смогу ответить позже."/utf8>>,
    <<"Это неизвестно."/utf8>>,
    %% maybe no
    <<"Очень сомнительно."/utf8>>,
    <<"Маловероятно."/utf8>>,
    <<"Не думаю, нет."/utf8>>,
    <<"Скорее всего нет."/utf8>>,
    %% strict no
    <<"Мой ответ — нет."/utf8>>,
    <<"Абсолютно точно - нет."/utf8>>,
    <<"Мои источники говорят \"нет\"."/utf8>>,
    <<"Нет, это невозможно."/utf8>>
]).

-record(state, {
    config :: marvin_plugin_config:t(),
    guild_id :: marvin_pdu2:snowflake()
}).
-type state() :: #state{}.



%% Interface



-spec get_commands(L10n :: binary()) ->
    Ret :: [marvin_plugin_command:t()].

get_commands(_) ->
    [guess()].



guess() ->
    marvin_plugin_command:new(#{
        plugin_id => <<"marvin_plugin_magic8">>,
        command => <<"guess">>,
        help => <<"Отвечает на простой вопрос (да/нет)."/utf8>>,
        keywords => [<<"вопрос"/utf8>>]
    }).



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_command(), marvin_plugin_command:short(guess())),
    {ok, #state{config = PluginConfig, guild_id = GuildId}}.



handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(Info, S0) ->
    try
        handle_info_guild_event(Info, S0)
    catch
        T:R:S ->
            ?l_error(#{
                text => "Plugin failed guild event handling",
                what => handle_info, result => fail,
                details => #{
                    type => T, reason => R, stacktrace => S,
                    guild_id => S0#state.guild_id
                }
            }),
            {noreply, S0}
    end.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



handle_info_guild_event(Event, S0) ->
    TypeCommand = marvin_guild_pubsub:type_command(),
    ShortCommandChangeNickname = marvin_plugin_command:short(guess()),
    case {marvin_guild_pubsub:type(Event), marvin_guild_pubsub:action(Event)} of
        {TypeCommand, ShortCommandChangeNickname} ->
            handle_info_guild_event_guess(Event, S0);
        {Type, Action} ->
            ?l_warning(#{
                text => "Plugin got unknown guild event",
                what => handle_info, result => fail,
                details => #{
                    pubsub_type => Type, pubsub_action => Action,
                    guild_id => S0#state.guild_id
                }
            }),
            {noreply, S0}
    end.



handle_info_guild_event_guess(Event, S0) ->
    #{original_message := OriginalMessage} = marvin_guild_pubsub:payload(Event),
    Content = generate_guess(),
    Req = marvin_rest2_request:new(
        marvin_rest2_impl_message_create,
        #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
        #{
            content => Content,
            message_reference => #{
                message_id => marvin_pdu2_dispatch_message_create:id(OriginalMessage),
                channel_id => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
                guild_id => S0#state.guild_id
            },
            allowed_mentions => #{replied_user => true}
        }
    ),
    _ = marvin_rest2:enqueue_request(Req),
    {noreply, S0}.



generate_guess() ->
    lists:nth(rand:uniform(erlang:length(?guess_answers())), ?guess_answers()).
