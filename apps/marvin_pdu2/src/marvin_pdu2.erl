-module(marvin_pdu2).
-compile({parse_transform, cloak_transform}).

-record(?MODULE, {
    op :: operation(),
    d = #{} :: data(),
    s = undefined :: sequence() | undefined,
    t = undefined :: event() | undefined,
    prot_mod :: atom()
}).

-export([parse/1, render/1, nullify/1]).


%% Defines


-define(op_dispatch, 0).
-define(op_heartbeat, 1).
-define(op_identify, 2).
-define(op_status_update, 3).
-define(op_voice_state_update, 4).
-define(op_voice_server_ping, 5).
-define(op_resume, 6).
-define(op_reconnect, 7).
-define(op_request_guild_members, 8).
-define(op_invalid_session, 9).
-define(op_hello, 10).
-define(op_heartbeat_ack, 11).
-define(ops, lists:seq(?op_dispatch, ?op_heartbeat_ack)).

-define(event_ready, <<"READY">>).
-define(event_resumed, <<"RESUMED">>).
-define(event_channel_create, <<"CHANNEL_CREATE">>).
-define(event_channel_update, <<"CHANNEL_UPDATE">>).
-define(event_channel_delete, <<"CHANNEL_DELETE">>).
-define(event_guild_create, <<"GUILD_CREATE">>).
-define(event_guild_update, <<"GUILD_UPDATE">>).
-define(event_guild_delete, <<"GUILD_DELETE">>).
-define(event_guild_ban_add, <<"GUILD_BAN_ADD">>).
-define(event_guild_ban_remove, <<"GUILD_BAN_REMOVE">>).
-define(event_guild_emojis_update, <<"GUILD_EMOJIS_UPDATE">>).
-define(event_guild_integrations_update, <<"GUILD_INTEGRATIONS_UPDATE">>).
-define(event_guild_member_add, <<"GUILD_MEMBER_ADD">>).
-define(event_guild_member_remove, <<"GUILD_MEMBER_REMOVE">>).
-define(event_guild_member_update, <<"GUILD_MEMBER_UPDATE">>).
-define(event_guild_members_chunk, <<"GUILD_MEMBERS_CHUNK">>).
-define(event_guild_role_create, <<"GUILD_ROLE_CREATE">>).
-define(event_guild_role_update, <<"GUILD_ROLE_UPDATE">>).
-define(event_guild_role_delete, <<"GUILD_ROLE_DELETE">>).
-define(event_message_create, <<"MESSAGE_CREATE">>).
-define(event_message_update, <<"MESSAGE_UPDATE">>).
-define(event_message_delete, <<"MESSAGE_DELETE">>).
-define(event_message_delete_bulk, <<"MESSAGE_DELETE_BULK">>).
-define(event_message_reaction_add, <<"MESSAGE_REACTION_ADD">>).
-define(event_message_reaction_remove, <<"MESSAGE_REACTION_REMOVE">>).
-define(event_message_reaction_remove_all, <<"MESSAGE_REACTION_REMOVE_ALL">>).
-define(event_presence_update, <<"PRESENCE_UPDATE">>).
-define(event_typing_start, <<"TYPING_START">>).
-define(event_user_update, <<"USER_UPDATE">>).
-define(event_voice_state_update, <<"VOICE_STATE_UPDATE">>).
-define(event_voice_server_update, <<"VOICE_SERVER_UPDATE">>).
-define(events, [
    ?event_ready, ?event_resumed, ?event_channel_create, ?event_channel_update, ?event_channel_delete,
    ?event_guild_create, ?event_guild_update, ?event_guild_delete, ?event_guild_ban_add,
    ?event_guild_ban_remove, ?event_guild_emojis_update, ?event_guild_integrations_update,
    ?event_guild_member_add, ?event_guild_member_remove, ?event_guild_member_update,
    ?event_guild_members_chunk, ?event_guild_role_create, ?event_guild_role_update,
    ?event_guild_role_delete, ?event_message_create, ?event_message_update, ?event_message_delete,
    ?event_message_delete_bulk, ?event_message_reaction_add, ?event_message_reaction_remove,
    ?event_message_reaction_remove_all, ?event_presence_update, ?event_typing_start,
    ?event_user_update, ?event_voice_state_update, ?event_voice_server_update
]).


%% Types


-type t() :: #?MODULE{}.

-type operation() :: ?op_dispatch..?op_heartbeat_ack.
-type data() :: term(). %% to be defined later
-type sequence() :: pos_integer().
-type event() :: binary().

-type trace_part() :: unicode:unicode_binary().
-type trace() :: [trace_part(), ...].

-type shard() :: non_neg_integer().
-type total_shards() :: pos_integer().
-type shard_spec() :: [shard() | total_shards()].

-type token() :: unicode:unicode_binary().
-type session_id() :: unicode:unicode_binary().

-type snowflake() :: unicode:unicode_binary().

-export_type([
    operation/0, data/0, sequence/0, event/0,
    trace_part/0, trace/0, shard/0, total_shards/0,
    shard_spec/0, token/0, session_id/0, snowflake/0
]).


%% Interface


-spec parse(Binary :: binary()) ->
    marvin_helper_type:generic_return(
        OkRet :: t(),
        ErrorRet ::
            {jiffy_error, term()} |
            not_implemented | invalid_op | term()
    ).

parse(Binary) ->
    marvin_helper_chain:chain('marvin_pdu2:parse', [
        fun decode_json/1,
        fun construct_internal/1
    ], Binary).


-spec render(Struct :: t()) ->
    marvin_helper_type:generic_return(
        OkRet :: binary(),
        ErrorRet :: term()
    ).

render(Struct) ->
    marvin_helper_chain:chain('marvin_pdu2:render', [
        fun detect_struct_attrs/1,
        fun construct_external/1,
        fun render_json/1
    ], Struct).


-spec nullify(Term :: term()) ->
    Ret :: null | term().

nullify(undefined) -> null;
nullify(Term) -> Term.


%% Internals


-spec decode_json(Binary :: binary()) ->
    marvin_helper_type:generic_return(
        OkRet :: #{},
        ErrorRet :: {jiffy_error, term()}
    ).

decode_json(Binary) ->
    try
        Message = jiffy:decode(Binary, [return_maps]),
        {ok, Message}
    catch
        _Type:Error -> {error, {jiffy_error, Error}}
    end.


-spec construct_internal(Message :: #{}) ->
    marvin_helper_type:ok_return(
        OkRet :: t()
    ) | no_return().

construct_internal(Message) ->
    {ok, ?MODULE:new(Message)}.


-spec detect_struct_attrs(Struct :: t()) ->
    marvin_helper_type:ok_return(
        OkRet :: {
            {Mod :: atom(), Op :: operation(), Event :: event()},
            Struct :: t()
        }
    ).

detect_struct_attrs(Struct) when is_tuple(Struct) ->
    % This is a very dirty hack obviously. To be fixed later (?).
    Mod = element(1, Struct),
    {Op, Event} = mod_to_op_event(Mod),
    {ok, {{Mod, Op, Event}, Struct}}.


-spec construct_external({
    {Mod :: atom(), Op :: operation(), Event :: event()},
    Struct :: t()
}) ->
    marvin_helper_type:ok_return(OkRet :: map()).

construct_external({{Mod, Op, Event}, Struct}) ->
    {ok, #{
        op => Op,
        d => Mod:export(Struct),
        t => Event
    }}.


-spec render_json(External :: map()) ->
    marvin_helper_type:ok_return(OkRet :: binary()).

render_json(External) ->
    {ok, jiffy:encode(External)}.


%% Cloak callbacks


cloak_validate(op, Value) ->
    case lists:member(Value, ?ops) of
        true ->
            {ok, Value};
        false ->
            {error, invalid}
    end;


cloak_validate(d, Value)
when null == Value orelse undefined == Value orelse false == Value ->
    {ok, #{}};

cloak_validate(d, Value) ->
    case Value of
        _ when is_map(Value) ->
            {ok, Value};
        _ ->
            {ok, #{<<"plain_value">> => Value}}
    end;

cloak_validate(s, Value)
when null == Value orelse undefined == Value ->
    {ok, undefined};

cloak_validate(s, Value)
when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(t, Value)
when null == Value orelse undefined == Value ->
    {ok, undefined};

cloak_validate(t, Value) when is_binary(Value) ->
    case lists:member(Value, ?events) of
        true ->
            {ok, Value};
        false ->
            {error, invalid}
    end;

cloak_validate(_, _) ->
    {error, invalid}.


cloak_validate_struct(#?MODULE{op = ?op_dispatch, d = Data, t = Event} = Struct)
when Event /= undefined ->
    % generic 'dispatch' op
    Mod = dispatch_event_to_mod(Event),
    {ok, Struct#?MODULE{prot_mod = Mod, d = cloak_validate_struct_construct_data_safe_temp(Mod, Data)}};

cloak_validate_struct(#?MODULE{op = Op, d = Data, t = undefined} = Struct) ->
    % any other case
    Mod = op_to_mod(Op),
    {ok, Struct#?MODULE{prot_mod = Mod, d = cloak_validate_struct_construct_data_safe_temp(Mod, Data)}};

cloak_validate_struct(_Struct) ->
    {error, invalid}.


cloak_validate_struct_construct_data_safe_temp(Mod, Data) ->
    try
        Mod:new(Data)
    catch
        _:_ ->
            spawn(fun() ->
                {ok, LogDir} = marvin_config:get(lager, log_root),
                Filename = filename:join([LogDir, io_lib:format("dump_~p_~p.json", [Mod, marvin_helper_time:timestamp()])]),
                marvin_log:warn("Failed to construct internal of mod '~p', dumping to '~s'.", [Mod, Filename]),
                file:write_file(Filename, jiffy:encode(Data))
            end),
            #{}
    end.


%% Mappings


dispatch_event_to_mod(?event_ready) -> marvin_pdu2_dispatch_ready;
dispatch_event_to_mod(?event_resumed) -> marvin_pdu2_dispatch_resumed;
dispatch_event_to_mod(?event_channel_create) -> marvin_pdu2_dispatch_channel_create;
dispatch_event_to_mod(?event_channel_update) -> marvin_pdu2_dispatch_channel_update;
dispatch_event_to_mod(?event_channel_delete) -> marvin_pdu2_dispatch_channel_delete;
dispatch_event_to_mod(?event_guild_create) -> marvin_pdu2_dispatch_guild_create;
dispatch_event_to_mod(?event_guild_update) -> marvin_pdu2_dispatch_guild_update;
dispatch_event_to_mod(?event_guild_delete) -> marvin_pdu2_dispatch_guild_delete;
dispatch_event_to_mod(?event_guild_ban_add) -> marvin_pdu2_dispatch_guild_ban_add;
dispatch_event_to_mod(?event_guild_ban_remove) -> marvin_pdu2_dispatch_guild_ban_remove;
dispatch_event_to_mod(?event_guild_emojis_update) -> marvin_pdu2_dispatch_guild_emojis_update;
dispatch_event_to_mod(?event_guild_integrations_update) -> marvin_pdu2_dispatch_guild_integrations_update;
dispatch_event_to_mod(?event_guild_member_add) -> marvin_pdu2_dispatch_guild_member_add;
dispatch_event_to_mod(?event_guild_member_remove) -> marvin_pdu2_dispatch_guild_member_remove;
dispatch_event_to_mod(?event_guild_member_update) -> marvin_pdu2_dispatch_guild_member_update;
dispatch_event_to_mod(?event_guild_members_chunk) -> marvin_pdu2_dispatch_guild_members_chunk;
dispatch_event_to_mod(?event_guild_role_create) -> marvin_pdu2_dispatch_guild_role_create;
dispatch_event_to_mod(?event_guild_role_update) -> marvin_pdu2_dispatch_guild_role_update;
dispatch_event_to_mod(?event_guild_role_delete) -> marvin_pdu2_dispatch_guild_role_delete;
dispatch_event_to_mod(?event_message_create) -> marvin_pdu2_dispatch_message_create;
dispatch_event_to_mod(?event_message_update) -> marvin_pdu2_dispatch_message_update;
dispatch_event_to_mod(?event_message_delete) -> marvin_pdu2_dispatch_message_delete;
dispatch_event_to_mod(?event_message_delete_bulk) -> marvin_pdu2_dispatch_message_delete_bulk;
dispatch_event_to_mod(?event_message_reaction_add) -> marvin_pdu2_dispatch_message_reaction_add;
dispatch_event_to_mod(?event_message_reaction_remove) -> marvin_pdu2_dispatch_message_reaction_remove;
dispatch_event_to_mod(?event_message_reaction_remove_all) -> marvin_pdu2_dispatch_message_reaction_remove_all;
dispatch_event_to_mod(?event_presence_update) -> marvin_pdu2_dispatch_presence_update;
dispatch_event_to_mod(?event_typing_start) -> marvin_pdu2_dispatch_typing_start;
dispatch_event_to_mod(?event_user_update) -> marvin_pdu2_dispatch_user_update;
dispatch_event_to_mod(?event_voice_state_update) -> marvin_pdu2_dispatch_voice_state_update;
dispatch_event_to_mod(?event_voice_server_update) -> marvin_pdu2_dispatch_voice_server_update.


op_to_mod(?op_heartbeat) -> marvin_pdu2_heartbeat;
op_to_mod(?op_identify) -> marvin_pdu2_identify;
op_to_mod(?op_status_update) -> marvin_pdu2_status_update;
op_to_mod(?op_voice_state_update) -> marvin_pdu2_voice_state_update;
op_to_mod(?op_voice_server_ping) -> marvin_pdu2_voice_server_ping;
op_to_mod(?op_resume) -> marvin_pdu2_resume;
op_to_mod(?op_reconnect) -> marvin_pdu2_reconnect;
op_to_mod(?op_request_guild_members) -> marvin_pdu2_request_guild_members;
op_to_mod(?op_invalid_session) -> marvin_pdu2_invalid_session;
op_to_mod(?op_hello) -> marvin_pdu2_hello;
op_to_mod(?op_heartbeat_ack) -> marvin_pdu2_heartbeat_ack.


mod_to_op_event(marvin_pdu2_heartbeat) -> {?op_heartbeat, undefined};
mod_to_op_event(marvin_pdu2_identify) -> {?op_identify, undefined};
mod_to_op_event(marvin_pdu2_status_update) -> {?op_status_update, undefined};
mod_to_op_event(marvin_pdu2_voice_state_update) -> {?op_voice_state_update, undefined};
mod_to_op_event(marvin_pdu2_voice_server_ping) -> {?op_voice_server_ping, undefined};
mod_to_op_event(marvin_pdu2_resume) -> {?op_resume, undefined};
mod_to_op_event(marvin_pdu2_reconnect) -> {?op_reconnect, undefined};
mod_to_op_event(marvin_pdu2_request_guild_members) -> {?op_request_guild_members, undefined};
mod_to_op_event(marvin_pdu2_invalid_session) -> {?op_invalid_session, undefined};
mod_to_op_event(marvin_pdu2_hello) -> {?op_hello, undefined};
mod_to_op_event(marvin_pdu2_heartbeat_ack) -> {?op_heartbeat_ack, undefined};

mod_to_op_event(marvin_pdu2_dispatch_ready) -> {?op_dispatch, ?event_ready};
mod_to_op_event(marvin_pdu2_dispatch_resumed) -> {?op_dispatch, ?event_resumed};
mod_to_op_event(marvin_pdu2_dispatch_channel_create) -> {?op_dispatch, ?event_channel_create};
mod_to_op_event(marvin_pdu2_dispatch_channel_update) -> {?op_dispatch, ?event_channel_update};
mod_to_op_event(marvin_pdu2_dispatch_channel_delete) -> {?op_dispatch, ?event_channel_delete};
mod_to_op_event(marvin_pdu2_dispatch_guild_create) -> {?op_dispatch, ?event_guild_create};
mod_to_op_event(marvin_pdu2_dispatch_guild_update) -> {?op_dispatch, ?event_guild_update};
mod_to_op_event(marvin_pdu2_dispatch_guild_delete) -> {?op_dispatch, ?event_guild_delete};
mod_to_op_event(marvin_pdu2_dispatch_guild_ban_add) -> {?op_dispatch, ?event_guild_ban_add};
mod_to_op_event(marvin_pdu2_dispatch_guild_ban_remove) -> {?op_dispatch, ?event_guild_ban_remove};
mod_to_op_event(marvin_pdu2_dispatch_guild_emojis_update) -> {?op_dispatch, ?event_guild_emojis_update};
mod_to_op_event(marvin_pdu2_dispatch_guild_integrations_update) -> {?op_dispatch, ?event_guild_integrations_update};
mod_to_op_event(marvin_pdu2_dispatch_guild_member_add) -> {?op_dispatch, ?event_guild_member_add};
mod_to_op_event(marvin_pdu2_dispatch_guild_member_remove) -> {?op_dispatch, ?event_guild_member_remove};
mod_to_op_event(marvin_pdu2_dispatch_guild_member_update) -> {?op_dispatch, ?event_guild_member_update};
mod_to_op_event(marvin_pdu2_dispatch_guild_members_chunk) -> {?op_dispatch, ?event_guild_members_chunk};
mod_to_op_event(marvin_pdu2_dispatch_guild_role_create) -> {?op_dispatch, ?event_guild_role_create};
mod_to_op_event(marvin_pdu2_dispatch_guild_role_update) -> {?op_dispatch, ?event_guild_role_update};
mod_to_op_event(marvin_pdu2_dispatch_guild_role_delete) -> {?op_dispatch, ?event_guild_role_delete};
mod_to_op_event(marvin_pdu2_dispatch_message_create) -> {?op_dispatch, ?event_message_create};
mod_to_op_event(marvin_pdu2_dispatch_message_update) -> {?op_dispatch, ?event_message_update};
mod_to_op_event(marvin_pdu2_dispatch_message_delete) -> {?op_dispatch, ?event_message_delete};
mod_to_op_event(marvin_pdu2_dispatch_message_delete_bulk) -> {?op_dispatch, ?event_message_delete_bulk};
mod_to_op_event(marvin_pdu2_dispatch_message_reaction_add) -> {?op_dispatch, ?event_message_reaction_add};
mod_to_op_event(marvin_pdu2_dispatch_message_reaction_remove) -> {?op_dispatch, ?event_message_reaction_remove};
mod_to_op_event(marvin_pdu2_dispatch_message_reaction_remove_all) -> {?op_dispatch, ?event_message_reaction_remove_all};
mod_to_op_event(marvin_pdu2_dispatch_presence_update) -> {?op_dispatch, ?event_presence_update};
mod_to_op_event(marvin_pdu2_dispatch_typing_start) -> {?op_dispatch, ?event_typing_start};
mod_to_op_event(marvin_pdu2_dispatch_user_update) -> {?op_dispatch, ?event_user_update};
mod_to_op_event(marvin_pdu2_dispatch_voice_state_update) -> {?op_dispatch, ?event_voice_state_update};
mod_to_op_event(marvin_pdu2_dispatch_voice_server_update) -> {?op_dispatch, ?event_voice_server_update}.
