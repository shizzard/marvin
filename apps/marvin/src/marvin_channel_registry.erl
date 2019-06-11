-module(marvin_channel_registry).
-behaviour(gen_server).
-include_lib("marvin_log/include/marvin_log.hrl").
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
    ?l_debug(#{
        text => "Register channel for guild",
        what => register,
        details => #{guild_id => GuildId, channel_id => ChannelId}
    }),
    gen_server:call(?MODULE, ?register(GuildId, ChannelId)).



-spec unregister(GuildId :: marvin_pdu2:snowflake(), ChannelId :: marvin_pdu2:snowflake()) ->
    Ret :: marvin_helper_type:ok_return().

unregister(GuildId, ChannelId) ->
    ?l_debug(#{
        text => "Unregister channel for guild",
        what => unregister,
        details => #{guild_id => GuildId, channel_id => ChannelId}
    }),
    gen_server:call(?MODULE, ?unregister(GuildId, ChannelId)).



-spec lookup(ChannelId :: marvin_pdu2:snowflake()) ->
    Ret :: marvin_helper_type:generic_return(
        OkRet :: marvin_pdu2:snowflake(),
        ErrorRet :: not_found
    ).

lookup(ChannelId) ->
    case ets:lookup(?MODULE, ChannelId) of
        [#item{channel_id = ChannelId, guild_id = GuildId}] ->
            ?l_debug(#{
                text => "Lookup channel",
                what => lookup, result => ok,
                details => #{guild_id => GuildId, channel_id => ChannelId}
            }),
            {ok, GuildId};
        [] ->
            ?l_debug(#{
                text => "Lookup channel",
                what => lookup, result => not_found,
                details => #{channel_id => ChannelId}
            }),
            {error, not_found}
    end.



init([]) ->
    {ok, #state{registry = ets:new(?MODULE, [
        set, named_table, protected, {keypos, 2}, {read_concurrency, true}
    ])}}.



%% Internals



handle_call(?register(GuildId, ChannelId), _GenReplyTo, S0) ->
    true = ets:insert(?MODULE, #item{channel_id = ChannelId, guild_id = GuildId}),
    ?l_info(#{
        text => "Channel registered for guild",
        what => handle_call_register, result => ok,
        details => #{guild_id => GuildId, channel_id => ChannelId}
    }),
    {reply, ok, S0};

handle_call(?unregister(GuildId, ChannelId), _GenReplyTo, S0) ->
    true = ets:delete(?MODULE, ChannelId),
    ?l_info(#{
        text => "Channel unregistered for guild",
        what => handle_call_unregister, result => ok,
        details => #{guild_id => GuildId, channel_id => ChannelId}
    }),
    {reply, ok, S0};

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
