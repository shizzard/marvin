-module(marvin_plugin_gay_of_the_day).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(active_user, {
    user_id :: marvin_pdu2:snowflake(),
    user :: marvin_pdu2_object_user:t()
}).

-record(handle_info_award_gay_of_the_day, {
    active_users :: ets:tid(),
    guild_id :: marvin_pdu2:snowflake(),
    awards_channel_id :: marvin_pdu2:snowflake(),
    awards_message_template :: unicode:unicode_binary(),
    awarded_user :: #active_user{} | undefined
}).

-record(state, {
    config :: marvin_plugin_config:t(),
    guild_id :: marvin_pdu2:snowflake(),
    active_users :: ets:tid(),
    timer :: timer:tref()
}).
-type state() :: #state{}.

-define(award_gay_of_the_day(), {award_gay_of_the_day}).



%% Interface



-spec get_commands(L10n :: binary()) ->
    Ret :: [].

get_commands(_) ->
    [].



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_message(), marvin_guild_pubsub:action_create()),
    {ok, #state{
        config = PluginConfig,
        guild_id = GuildId,
        active_users = ets:new(marvin_plugin_gay_of_the_day_active_users, [
            set, public, named_table, {keypos, 2}, {read_concurrency, true}
        ]),
        timer = init_timer(marvin_plugin_config:data(PluginConfig))
    }}.



handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(?award_gay_of_the_day(), S0) ->
    handle_info_award_gay_of_the_day(S0);

handle_info(Info, S0) ->
    try
        handle_info_guild_event(Info, S0)
    catch
        T:R ->
            marvin_log:error(
                "Guild '~s' plugin '~s' failed guild event hadling with reason ~p:~p: ~p",
                [S0#state.guild_id, ?MODULE, T, R, erlang:get_stacktrace()]
            ),
            {noreply, S0}
    end.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



init_timer(#{
    <<"hours">> := Hours,
    <<"minutes">> := Minutes
}) ->
    erlang:send_after(seconds_to_desired_time(Hours, Minutes) * 1000, self(), ?award_gay_of_the_day()).



seconds_to_desired_time(Hours, Minutes) ->
    {NowDate, NowTime} = calendar:universal_time(),
    DesiredTime = {Hours, Minutes, 0},
    case {
        calendar:datetime_to_gregorian_seconds({NowDate, NowTime}),
        calendar:datetime_to_gregorian_seconds({NowDate, DesiredTime})
    } of
        {Now, Future} when Now < Future ->
            Future - Now;
        {Now, Past} when Now >= Past ->
            FutureDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(NowDate) + 1),
            Future = calendar:datetime_to_gregorian_seconds({FutureDate, DesiredTime}),
            Future - Now
    end.



handle_info_award_gay_of_the_day(S0) ->
    #{
        <<"awards_channel_id">> := ChannelId,
        <<"awards_message_template">> := MessageTemplate
    } = marvin_plugin_config:data(S0#state.config),
    case marvin_helper_chain:chain('marvin_plugin_gay_of_the_day:handle_info_award_gay_of_the_day', [
        fun handle_info_award_gay_of_the_day_select_awarded_user/1,
        fun handle_info_award_gay_of_the_day_send_message/1,
        fun handle_info_award_gay_of_the_day_reset_ets_table/1,
        fun handle_info_award_gay_of_the_day_persist_award/1
    ], #handle_info_award_gay_of_the_day{
        guild_id = S0#state.guild_id,
        active_users = S0#state.active_users,
        awards_channel_id = ChannelId,
        awards_message_template = MessageTemplate
    }) of
        {ok, _ChainCtx} ->
            marvin_log:info(
                "Guild '~s' plugin '~s' awarded the gay-of-the-day prize",
                [S0#state.guild_id, ?MODULE]
            );
        {skip, Reason} ->
            marvin_log:info(
                "Guild '~s' plugin '~s' skipped the gay-of-the-day award due to '~p'",
                [S0#state.guild_id, ?MODULE, Reason]
            );
        {error, Reason} ->
            marvin_log:info(
                "Guild '~s' plugin '~s' failed the gay-of-the-day award due to '~p'",
                [S0#state.guild_id, ?MODULE, Reason]
            )
    end,
    {noreply, S0#state{timer = init_timer(marvin_plugin_config:data(S0#state.config))}}.



handle_info_award_gay_of_the_day_select_awarded_user(#handle_info_award_gay_of_the_day{
    active_users = ActiveUsers
} = ChainCtx) ->
    case ets:tab2list(ActiveUsers) of
        [] ->
            {skip, no_active_users};
        Users ->
            {ok, ChainCtx#handle_info_award_gay_of_the_day{
                awarded_user = lists:nth(rand:uniform(length(Users)), Users)
            }}
    end.



handle_info_award_gay_of_the_day_send_message(#handle_info_award_gay_of_the_day{
    awarded_user = AwardedUser,
    awards_channel_id = ChannelId,
    awards_message_template = MessageTemplate
} = ChainCtx) ->
    Req = marvin_rest_request:new(
        marvin_rest_impl_message_create,
        #{<<"channel_id">> => ChannelId},
        #{content => binary:replace(
            MessageTemplate,
            <<"{{user_mention}}">>,
            marvin_pdu2_object_user:format(AwardedUser#active_user.user),
            [global]
        )}
    ),
    Resp = marvin_rest:request(Req),
    marvin_log:info("Response: ~p", [Resp]),
    {ok, ChainCtx}.



handle_info_award_gay_of_the_day_reset_ets_table(#handle_info_award_gay_of_the_day{
    active_users = ActiveUsers
} = ChainCtx) ->
    true = ets:delete_all_objects(ActiveUsers),
    {ok, ChainCtx}.



handle_info_award_gay_of_the_day_persist_award(#handle_info_award_gay_of_the_day{
    guild_id = GuildId,
    awarded_user = AwardedUser
} = ChainCtx) ->
    AlreadyAwardedTimes = case marvin_storage:get_user_info(?MODULE, GuildId, AwardedUser#active_user.user_id) of
        undefined -> 0;
        #{<<"awards">> := Times} -> Times
    end,
    marvin_storage:set_user_info(
        ?MODULE, GuildId, AwardedUser#active_user.user_id,
        #{<<"awards">> => AlreadyAwardedTimes + 1}
    ),
    {ok, ChainCtx}.



handle_info_guild_event(Event, S0) ->
    Author = marvin_pdu2_dispatch_message_create:author(marvin_guild_pubsub:payload(Event)),
    ets:insert(S0#state.active_users, #active_user{
        user_id = marvin_pdu2_object_user:id(Author),
        user = Author
    }),
    {noreply, S0}.
