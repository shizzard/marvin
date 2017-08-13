-module(marvin_guild_monitor).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([maybe_start_guild/1, start_guild/1, stop_guild/1, get_guild/1]).



-define(maybe_start_guild(GuildId), {maybe_start_guild, GuildId}).
-define(start_guild(GuildId), {start_guild, GuildId}).
-define(stop_guild(GuildId), {stop_guild, GuildId}).
-define(get_guild(GuildId), {get_guild, GuildId}).
-define(get_guilds(), {get_guilds}).

-record(state, {
    registry :: ets:tid()
}).
-type state() :: #state{}.



%% Interface



-spec maybe_start_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

maybe_start_guild(GuildId) ->
    gen_server:call(?MODULE, ?maybe_start_guild(GuildId)).



-spec start_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_guild(GuildId) ->
    gen_server:call(?MODULE, ?start_guild(GuildId)).



-spec stop_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return().

stop_guild(GuildId) ->
    gen_server:call(?MODULE, ?stop_guild(GuildId)).



-spec get_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

get_guild(GuildId) ->
    gen_server:call(?MODULE, ?get_guild(GuildId)).



-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    Registry = ets:new(marvin_guild_monitor_registry, [
        set, protected, named_table, {keypos, 2}, {read_concurrency, true}
    ]),
    {ok, #state{registry = Registry}}.



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



%% Internals
