-module(marvin_guild_monitor).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([maybe_start_guild/2, start_guild/2, stop_guild/1, get_guild/1]).



-define(registry_table, marvin_guild_monitor_registry).
-define(start_guild(GuildId, MyId), {start_guild, GuildId, MyId}).
-define(stop_guild(GuildId), {stop_guild, GuildId}).
-define(monitor_down(MonRef, Pid, Info), {'DOWN', MonRef, process, Pid, Info}).

-record(guild, {
    my_id :: marvin_pdu2_object_user:id(),
    guild_id :: marvin_pdu2_object_guild:id(),
    guild_pid :: pid(),
    guild_mon_ref :: reference()
}).
-record(state, {
    registry :: ets:tid()
}).
-type state() :: #state{}.



%% Interface



-spec maybe_start_guild(GuildId :: marvin_pdu2:snowflake(), MyId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

maybe_start_guild(GuildId, MyId) ->
    case get_guild(GuildId) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            start_guild(GuildId, MyId)
    end.



-spec start_guild(GuildId :: marvin_pdu2:snowflake(), MyId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(
        OkRet :: pid(),
        ErrorRet :: duplicate
    ).

start_guild(GuildId, MyId) ->
    case get_guild(GuildId) of
        {ok, _Pid} ->
            {error, duplicate};
        {error, not_found} ->
            gen_server:call(?MODULE, ?start_guild(GuildId, MyId))
    end.



-spec stop_guild(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return().

stop_guild(GuildId) ->
    gen_server:call(?MODULE, ?stop_guild(GuildId)).



-spec get_guild(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: not_found
    ).

get_guild(GuildId) ->
    case ets:lookup(?registry_table, GuildId) of
        [#guild{guild_id = GuildId, guild_pid = Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.



-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    Registry = ets:new(?registry_table, [
        set, protected, named_table, {keypos, 3}, {read_concurrency, true}
    ]),
    {ok, #state{registry = Registry}}.



handle_call(?start_guild(GuildId, MyId), _GenReplyTo, S0) ->
    handle_call_start_guild(GuildId, MyId, S0);

handle_call(?stop_guild(GuildId), _GenReplyTo, S0) ->
    handle_call_stop_guild(GuildId, S0);

handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(?monitor_down(MonRef, Pid, Info), S0) ->
    handle_info_monitor_down(MonRef, Pid, Info, S0);

handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec handle_call_start_guild(
    GuildId :: marvin_pdu2:snowflake(),
    MyId :: marvin_pdu2:snowflake(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Ret :: marvin_helper_type:ok_return(OkRet :: pid()),
        State :: state()
    ).

handle_call_start_guild(GuildId, MyId, S0) ->
    {ok, Pid} = do_start_guild(GuildId, MyId),
    {reply, {ok, Pid}, S0}.



-spec handle_call_stop_guild(
    GuildId :: marvin_pdu2:snowflake(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Ret :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_stop_guild(GuildId, S0) ->
    case ets:lookup(?registry_table, GuildId) of
        [#guild{guild_id = GuildId, guild_pid = Pid, guild_mon_ref = MonRef}] ->
            _ = erlang:demonitor(MonRef, [flush, info]),
            ok = marvin_guild_sup:stop_guild(Pid),
            true = ets:delete(?registry_table, GuildId);
        [] ->
            ok
    end,
    {reply, ok, S0}.



-spec handle_info_monitor_down(
    MonRef :: erlang:reference(),
    Pid :: pid(),
    Info :: term(),
    State :: state()
) ->
    marvin_helper_type:gen_server_noreply_simple(State :: state()).

handle_info_monitor_down(MonRef, Pid, Info, S0) ->
    case ets:match_object(?registry_table, {'_', '_', '_', Pid, MonRef}) of
        [] ->
            marvin_log:warn(
                "Unexpected monitor DOWN from ~p with reason ~w",
                [Pid, Info]
            );
        [#guild{my_id = MyId, guild_id = GuildId, guild_pid = Pid, guild_mon_ref = MonRef}] ->
            marvin_log:warn(
                "Guild '~ts' (~p) terminated with reason ~w; restarting",
                [GuildId, Pid, Info]
            ),
            true = ets:delete(?registry_table, GuildId),
            {ok, _Pid} = do_start_guild(GuildId, MyId)
    end,
    {noreply, S0}.



do_start_guild(GuildId, MyId) ->
    {ok, Pid} = marvin_guild_sup:start_guild(GuildId, MyId),
    MonRef = erlang:monitor(process, Pid),
    true = ets:insert(?registry_table, #guild{
        my_id = MyId, guild_id = GuildId, guild_pid = Pid, guild_mon_ref = MonRef
    }),
    {ok, Pid}.
