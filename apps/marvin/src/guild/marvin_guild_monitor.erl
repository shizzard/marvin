-module(marvin_guild_monitor).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([maybe_start_guild/1, start_guild/1, stop_guild/1, get_guild/1]).



-define(registry_table_name, marvin_guild_monitor_registry).
-define(start_guild(GuildId), {start_guild, GuildId}).
-define(stop_guild(GuildId), {stop_guild, GuildId}).

-record(state, {
    registry :: ets:tid()
}).
-type state() :: #state{}.



%% Interface



-spec maybe_start_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

maybe_start_guild(GuildId) ->
    case get_guild(GuildId) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            start_guild(GuildId)
    end.



-spec start_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(
        OkRet :: pid(),
        ErrorRet :: duplicate
    ).

start_guild(GuildId) ->
    case get_guild(GuildId) of
        {ok, _Pid} ->
            {error, duplicate};
        {error, not_found} ->
            gen_server:call(?MODULE, ?start_guild(GuildId))
    end.



-spec stop_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return().

stop_guild(GuildId) ->
    gen_server:call(?MODULE, ?stop_guild(GuildId)).



-spec get_guild(GuildId :: non_neg_integer()) ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: not_found
    ).

get_guild(GuildId) ->
    case ets:lookup(?registry_table_name, GuildId) of
        [{GuildId, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.



-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    Registry = ets:new(?registry_table_name, [
        set, protected, named_table, {keypos, 1}, {read_concurrency, true}
    ]),
    {ok, #state{registry = Registry}}.



handle_call(?start_guild(GuildId), _GenReplyTo, S0) ->
    handle_call_start_guild(GuildId, S0);

handle_call(?stop_guild(GuildId), _GenReplyTo, S0) ->
    handle_call_stop_guild(GuildId, S0);

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



-spec handle_call_start_guild(GuildId :: non_neg_integer(), State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Ret :: marvin_helper_type:ok_return(OkRet :: pid()),
        State :: state()
    ).

handle_call_start_guild(GuildId, S0) ->
    {ok, Pid} = marvin_guild_sup:start_guild(GuildId),
    true = ets:insert(?registry_table_name, {GuildId, Pid}),
    {reply, {ok, Pid}, S0}.



-spec handle_call_stop_guild(GuildId :: non_neg_integer(), State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Ret :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_stop_guild(GuildId, S0) ->
    ok = marvin_guild_sup:stop_guild(GuildId),
    true = ets:delete(?registry_table_name, GuildId),
    {reply, ok, S0}.
