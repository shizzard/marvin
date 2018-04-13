-module(marvin_channel_registry).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    register/2, unregister/2, lookup/1,
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(item, {
    channel_id :: marvin_pdu2:snowflake(),
    guild_id :: marvin_pdu2:snowflake()
}).
-record(state, {
    registry :: ets:tid()
}).
-type state() :: #state{}.

-define(register(GuildId, ChannelId), {register, GuildId, ChannelId}).
-define(unregister(GuildId, ChannelId), {unregister, GuildId, ChannelId}).
-define(lookup(ChannelId), {lookup, ChannelId}).



%% Interface



-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



-spec register(GuildId :: marvin_pdu2:snowflake(), ChannelId :: marvin_pdu2:snowflake()) ->
    Ret :: marvin_helper_type:ok_return().

register(GuildId, ChannelId) ->
    gen_server:call(?MODULE, ?register(GuildId, ChannelId)).



-spec unregister(GuildId :: marvin_pdu2:snowflake(), ChannelId :: marvin_pdu2:snowflake()) ->
    Ret :: marvin_helper_type:ok_return().

unregister(GuildId, ChannelId) ->
    gen_server:call(?MODULE, ?unregister(GuildId, ChannelId)).



-spec lookup(ChannelId :: marvin_pdu2:snowflake()) ->
    Ret :: marvin_helper_type:generic_return(
        OkRet :: marvin_pdu2:snowflake(),
        ErrorRet :: not_found
    ).

lookup(ChannelId) ->
    case ets:lookup(?MODULE, ChannelId) of
        [#item{channel_id = ChannelId, guild_id = GuildId}] ->
            {ok, GuildId};
        [] ->
            {error, not_found}
    end.



init([]) ->
    {ok, #state{registry = ets:new(?MODULE, [
        set, named_table, protected, {keypos, 2}, {read_concurrency, true}
    ])}}.



%% Internals



handle_call(?register(GuildId, ChannelId), _GenReplyTo, S0) ->
    marvin_log:debug("Channel '~s' registered for guild '~s'", [ChannelId, GuildId]),
    true = ets:insert(?MODULE, #item{channel_id = ChannelId, guild_id = GuildId}),
    {reply, ok, S0};

handle_call(?unregister(GuildId, ChannelId), _GenReplyTo, S0) ->
    marvin_log:debug("Channel '~s' unregistered for guild '~s'", [ChannelId, GuildId]),
    true = ets:delete(?MODULE, ChannelId),
    {reply, ok, S0};

handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.
