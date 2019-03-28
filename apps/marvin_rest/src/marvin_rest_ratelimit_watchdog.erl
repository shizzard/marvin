-module(marvin_rest_ratelimit_watchdog).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([reserve_slot/1, update_limits/3]).

-define(reserve_slot(RatelimitGroup), {reserve_slot, RatelimitGroup}).
-define(
    update_limits(RatelimitGroup, SlotsAvailable, ReplenishAfter),
    {update_limits, RatelimitGroup, SlotsAvailable, ReplenishAfter}
).

-record(state, {
    group_guild_id_slots = 0 :: non_neg_integer(),
    group_guild_id_replenish_after = 0 :: non_neg_integer(),
    group_channel_id_slots = 0 :: non_neg_integer(),
    group_channel_id_replenish_after = 0 :: non_neg_integer(),
    group_webhook_id_slots = 0 :: non_neg_integer(),
    group_webhook_id_replenish_after = 0 :: non_neg_integer()
}).
-type state() :: #state{}.



%% Interface



reserve_slot(RatelimitGroup) ->
    gen_server:call(?MODULE, ?reserve_slot(RatelimitGroup)).



update_limits(RatelimitGroup, SlotsAvailable, ReplenishAfter) ->
    gen_server:call(?MODULE, ?update_limits(RatelimitGroup, SlotsAvailable, ReplenishAfter)).



-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    {ok, #state{}}.



handle_call(?reserve_slot(RatelimitGroup), _GenReplyTo, S0) ->
    handle_call_reserve_slot(RatelimitGroup, S0);

handle_call(?update_limits(RatelimitGroup, SlotsAvailable, ReplenishAfter), _GenReplyTo, S0) ->
    handle_call_update_limits(RatelimitGroup, SlotsAvailable, ReplenishAfter, S0);

handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected info", what => handle_info, details => Unexpected}),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



%% Lots of copypaste for sake of efficiency here, meh
handle_call_reserve_slot(guild_id, #state{
    group_guild_id_slots = Slots,
    group_guild_id_replenish_after = ReplenishAfter
} = S0) ->
    case marvin_helper_time:timestamp() of
        TS when TS =< ReplenishAfter andalso Slots == 0 ->
            %% No slots avalable, replenish time is in future: error
            {reply, {error, {retry_after, ReplenishAfter - TS}}, S0#state{group_guild_id_slots = Slots - 1}};
        TS when TS =< ReplenishAfter andalso Slots > 0 ->
            %% replenish time is in future, but we have some slots available: ok, go
            {reply, ok, S0#state{group_guild_id_slots = Slots - 1}};
        TS when TS > ReplenishAfter ->
            %% Probably this case may cause some 429 errors in case of very quick request burst;
            %% Will fix this after this start to happen though
            {reply, ok, S0}
    end;

handle_call_reserve_slot(channel_id, #state{
    group_channel_id_slots = Slots,
    group_channel_id_replenish_after = ReplenishAfter
} = S0) ->
    case marvin_helper_time:timestamp() of
        TS when TS =< ReplenishAfter andalso Slots =< 0 ->
            %% No slots avalable, replenish time is in future: error
            {reply, {error, {retry_after, ReplenishAfter - TS}}, S0#state{group_channel_id_slots = Slots - 1}};
        TS when TS =< ReplenishAfter andalso Slots > 0 ->
            %% replenish time is in future, but we have some slots available: ok, go
            {reply, ok, S0#state{group_channel_id_slots = Slots - 1}};
        TS when TS > ReplenishAfter ->
            %% Probably this case may cause some 429 errors in case of very quick request burst;
            %% Will fix this after this start to happen though
            {reply, ok, S0}
    end;

handle_call_reserve_slot(webhook_id, #state{
    group_webhook_id_slots = Slots,
    group_webhook_id_replenish_after = ReplenishAfter
} = S0) ->
    case marvin_helper_time:timestamp() of
        TS when TS =< ReplenishAfter andalso Slots == 0 ->
            %% No slots avalable, replenish time is in future: error
            {reply, {error, {retry_after, ReplenishAfter - TS}}, S0#state{group_webhook_id_slots = Slots - 1}};
        TS when TS =< ReplenishAfter andalso Slots > 0 ->
            %% replenish time is in future, but we have some slots available: ok, go
            {reply, ok, S0#state{group_webhook_id_slots = Slots - 1}};
        TS when TS > ReplenishAfter ->
            %% Probably this case may cause some 429 errors in case of very quick request burst;
            %% Will fix this after this start to happen though
            {reply, ok, S0}
    end.



handle_call_update_limits(guild_id, SlotsAvailable, ReplenishAfter, S0) ->
    {reply, ok, S0#state{
        group_guild_id_slots = SlotsAvailable,
        group_guild_id_replenish_after = ReplenishAfter
    }};

handle_call_update_limits(channel_id, SlotsAvailable, ReplenishAfter, S0) ->
    {reply, ok, S0#state{
        group_channel_id_slots = SlotsAvailable,
        group_channel_id_replenish_after = ReplenishAfter
    }};

handle_call_update_limits(webhook_id, SlotsAvailable, ReplenishAfter, S0) ->
    {reply, ok, S0#state{
        group_webhook_id_slots = SlotsAvailable,
        group_webhook_id_replenish_after = ReplenishAfter
    }}.
