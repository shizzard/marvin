-module(marvin_plugin_help).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
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
    [get_help()].



get_help() ->
    marvin_plugin_command:new(#{
        plugin_id => <<"marvin_plugin_help">>,
        command => <<"generic_help">>,
        help => <<"Выводит эту справку."/utf8>>,
        keywords => [<<"справка"/utf8>>, <<"помощь"/utf8>>]
    }).



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_command(), marvin_plugin_command:short(get_help())),
    {ok, #state{config = PluginConfig, guild_id = GuildId}}.



handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(Info, S0) ->
    try
        handle_info_guild_event(Info, S0)
    catch
        T:R ->
            marvin_log:error(
                "Guild '~s' plugin '~s' failed guild event hadling with reason ~p:~p. ~p",
                [S0#state.guild_id, ?MODULE, T, R, erlang:get_stacktrace()]
            ),
            {noreply, S0}
    end.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



handle_info_guild_event(Event, S0) ->
    TypeCommand = marvin_guild_pubsub:type_command(),
    ShortCommandChangeNickname = marvin_plugin_command:short(get_help()),
    case {marvin_guild_pubsub:type(Event), marvin_guild_pubsub:action(Event)} of
        {TypeCommand, ShortCommandChangeNickname} ->
            handle_info_guild_event_get_help(Event, S0);
        {_Type, _Action} ->
            marvin_log:warn(
                "Plugin '~s' for guild '~s' got unknown guild event: ~ts/~ts",
                [?MODULE, S0#state.guild_id, _Type, _Action]
            ),
            {noreply, S0}
    end.



handle_info_guild_event_get_help(Event, S0) ->
    marvin_log:info(
        "Plugin '~s' for guild '~s' is sending help",
        [?MODULE, S0#state.guild_id]
    ),
    {ok, GuildCtx} = marvin_guild:get_context(S0#state.guild_id),
    MyNickname = marvin_pdu2_object_member:get_nickname(
        marvin_guild_helper_members:r_get_member_by_user_id(
            marvin_guild_context:my_id(GuildCtx), GuildCtx
        )
    ),
    #{original_message := OriginalMessage} = marvin_guild_pubsub:payload(Event),
    {ok, LibraryWeb} = marvin_config:get(marvin, [system_info, library_web]),
    {ok, LibraryName} = marvin_config:get(marvin, [system_info, library_name]),
    {ok, LibraryVersion} = marvin_config:get(marvin, [system_info, library_version]),
    Title = <<LibraryName/binary, " v", LibraryVersion/binary>>,
    {_, Fields} = lists:foldl(
        fun handle_info_guild_event_get_help_command_fold/2,
        {MyNickname, []},
        marvin_guild_context:commands(GuildCtx)
    ),
    Req = marvin_rest_request:new(
        marvin_rest_impl_message_create,
        #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
        #{
            content => <<>>,
            embed => #{
                title => Title,
                url => LibraryWeb,
                type => <<"rich">>,
                fields => Fields
            }
        }
    ),
    Resp = marvin_rest:request(Req),
    marvin_log:info("Response: ~p", [Resp]),
    {noreply, S0}.



handle_info_guild_event_get_help_command_fold(Command, {MyNickname, Acc}) ->
    {MyNickname, [#{
        name => iolist_to_binary([["@", MyNickname, " "] | lists:join(<<"/">>, marvin_plugin_command:keywords(Command))]),
        value => marvin_plugin_command:help(Command),
        inline => false
    } | Acc]}.
