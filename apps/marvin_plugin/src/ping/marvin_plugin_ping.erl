-module(marvin_plugin_ping).
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
    [marvin_plugin_command:new(#{
        plugin_id => <<"marvin_plugin_ping">>,
        command => <<"status">>,
        help => <<"Проверка статуса бота."/utf8>>,
        keywords => [<<"статус"/utf8>>, <<"пинг"/utf8>>]
    })].



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    [
        marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_command(), marvin_plugin_command:short(Command))
        || Command <- get_commands(any)
    ],
    {ok, #state{
        config = PluginConfig,
        guild_id = GuildId
    }}.



%% Internals



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
                "Guild '~s' plugin '~s' failed guild event hadling with reason ~p:~p",
                [S0#state.guild_id, ?MODULE, T, R]
            ),
            {noreply, S0}
    end.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



handle_info_guild_event(Event, S0) ->
    #{original_message := OriginalMessage} = marvin_guild_pubsub:payload(Event),
    Req = marvin_rest_request:new(
        marvin_rest_impl_message_create,
        #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
        #{content => get_status_message()}
    ),
    Resp = marvin_rest:request(Req),
    marvin_log:info("Response: ~p", [Resp]),
    {noreply, S0}.



get_status_message() ->
    {ok, LibraryName} = marvin_config:get(marvin, [system_info, library_name]),
    {ok, LibraryVersion} = marvin_config:get(marvin, [system_info, library_version]),
    MemoryFootprintMB = erlang:memory(total) / (1024 * 1024),
    {
        StoragePoolStatus, StorageWorkersCount, StorageOverflowCount, StorageBusyCount
    } = poolboy:status(marvin_storage),
    Uptime = marvin_helper_time:uptime_string(),
    iolist_to_binary([
        "```",
        <<LibraryName/binary, " v", LibraryVersion/binary, " is up for ", Uptime/binary, "\n">>,
        "Storage ", io_lib:format("~s", [StoragePoolStatus]), ", ",
        "workers: ", io_lib:format("~B", [StorageWorkersCount]), ", ",
        "overflow: ", io_lib:format("~B", [StorageOverflowCount]), ", ",
        "busy: ", io_lib:format("~B", [StorageBusyCount]), "\n",
        "Memory footprint: ", io_lib:format("~.2f MB", [MemoryFootprintMB]), "\n"
        "```"
    ]).
