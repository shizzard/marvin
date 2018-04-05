-module(marvin_guild).
-behaviour(gen_server).

-include("marvin_guild_state.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    do_provision/2, presence_update/2,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(do_provision(Struct), {do_provision, Struct}).
-define(presence_update(Struct), {presence_update, Struct}).



%% Interface



-spec start_link(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



-spec do_provision(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_guild_create:t()) ->
    Ret :: marvin_helper_type:ok_return().

do_provision(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?do_provision(Struct)).



-spec presence_update(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_presence_update:t()) ->
    Ret :: marvin_helper_type:ok_return().

presence_update(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?presence_update(Struct)).



init([GuildId]) ->
    {ok, #state{
        guild_id = GuildId,
        presence_state = ets:new(marvin_guild_presence_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        role_state = ets:new(marvin_guild_role_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        emoji_state = ets:new(marvin_guild_emoji_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ])
    }}.



%% Internals



handle_call(?do_provision(Struct), _GenReplyTo, S0) ->
    marvin_log:info("Guild '~s' is being provisioned", [S0#state.guild_id]),
    {ok, {Struct, S1}} = marvin_helper_chain:chain('marvin_guild:handle_call_do_provision', [
        fun marvin_guild_helper_presence:handle_call_do_provision_chain/1,
        fun marvin_guild_helper_role:handle_call_do_provision_chain/1,
        fun marvin_guild_helper_emoji:handle_call_do_provision_chain/1
    ], {Struct, S0}),
    {reply, ok, S1};

handle_call(?presence_update(Struct), _GenReplyTo, S0) ->
    marvin_log:info("Guild '~s' got presence update", [S0#state.guild_id]),
    {ok, {Struct, S1}} = marvin_helper_chain:chain('marvin_guild:handle_call_do_provision', [
        fun marvin_guild_helper_presence:handle_call_presence_update_chain/1
    ], {Struct, S0}),
    {reply, ok, S1};

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
