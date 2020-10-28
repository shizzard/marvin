-module(marvin_plugin_wiki).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(words_to_use, 20).
-define(wiki_base_url, <<"https://ru.wikipedia.org">>).
-define(wiki_base_path, <<"/w/api.php">>).

-record(handle_info_guild_event, {
    parsed_message_content :: [string()],
    original_message :: marvin_pdu2_dispatch_message_create:t(),
    request_url :: term(),
    response_body :: binary()
}).

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
        plugin_id => <<"marvin_plugin_wiki">>,
        command => <<"search">>,
        help => <<"Быстрый поиск статьи в Википедии."/utf8>>,
        keywords => [<<"вики"/utf8>>]
    })].



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    hackney_pool:start_pool(?MODULE, [{timeout, 10000}, {max_connections, 20}]),
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



handle_info_guild_event(Event, S0) ->
    #{
        parsed_message_content := ParsedMessage,
        original_message := OriginalMessage
    } = marvin_guild_pubsub:payload(Event),
    case marvin_helper_chain:chain('marvin_plugin_wiki:handle_info_guild_event', [
        fun handle_info_guild_event_prepare_query/1,
        fun handle_info_guild_event_perform_query/1,
        fun handle_info_guild_event_send_message/1
    ], #handle_info_guild_event{
        parsed_message_content = ParsedMessage,
        original_message = OriginalMessage
    }) of
        {ok, _Ctx} ->
            {noreply, S0};
        {error, Reason} ->
            ?l_error(#{
                text => "Plugin failed to perform wiki command",
                what => handle_info, result => error,
                details => #{
                    guild_id => S0#state.guild_id,
                    type => error, reason => Reason
                }
            }),
            {noreply, S0}
    end.



handle_info_guild_event_prepare_query(#handle_info_guild_event{
    parsed_message_content = ParsedMessage
} = Ctx) ->
    Words = [
        case Part of
            Part when is_integer(Part) -> integer_to_list(Part);
            Part -> Part
        end  || Part <- ParsedMessage,
        not is_tuple(Part), Part /= <<"вики"/utf8>>
    ],
    SearchQuery = case length(Words) of
        N when N >= ?words_to_use ->
            {SearchWords, _Rest} = lists:split(?words_to_use, Words),
            iolist_to_binary(lists:join(" ", SearchWords));
        _ ->
            iolist_to_binary(lists:join(" ", Words))
    end,
    %% ?action\=opensearch\&format\=json\&search\=<SEARCH>\&limit\=1\&redirects\=resolve
    Url = hackney_url:make_url(?wiki_base_url, ?wiki_base_path, [
        {<<"action">>, <<"opensearch">>},
        {<<"format">>, <<"json">>},
        {<<"limit">>, <<"1">>},
        {<<"redirects">>, <<"resolve">>},
        {<<"search">>, SearchQuery}
    ]),
    ?l_alert(#{
        text => "Wikipedia API url",
        what => handle_info,
        details => Url
    }),
    {ok, Ctx#handle_info_guild_event{request_url = Url}}.



handle_info_guild_event_perform_query(#handle_info_guild_event{
    request_url = Url
} = Ctx) ->
    case hackney:request(get, Url) of
        {ok, 200, _, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, Ctx#handle_info_guild_event{response_body = jsone:decode(Body)}};
        {error, Reason} ->
            {error, Reason}
    end.



handle_info_guild_event_send_message(#handle_info_guild_event{
    response_body = Body,
    original_message = OriginalMessage
} = Ctx) ->
    case Body of
        [SearchQuery, [], [], []] ->
            marvin_rest2:enqueue_request(marvin_rest2_request:new(
                marvin_rest2_impl_message_create,
                #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
                #{
                    content => iolist_to_binary([
                        <<"По запросу '"/utf8>>, SearchQuery, <<"' статей в Википедии нет."/utf8>>])
                }
            ));
        [SearchQuery, [Header], [Text], [Link]] ->
            marvin_rest2:enqueue_request(marvin_rest2_request:new(
                marvin_rest2_impl_message_create,
                #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
                #{
                    content => iolist_to_binary([
                        <<"Вот что я нашел в Википедии по запросу '"/utf8>>, SearchQuery, <<"':"/utf8>>]),
                    embed => #{title => Header, url => Link, description => Text}
                }
            ));
        _ ->
            marvin_rest2:enqueue_request(marvin_rest2_request:new(
                marvin_rest2_impl_message_create,
                #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
                #{
                    content => iolist_to_binary([
                        <<"Чота сломалось. Почините."/utf8>>])
                }
            ))
    end,
    {ok, Ctx}.
