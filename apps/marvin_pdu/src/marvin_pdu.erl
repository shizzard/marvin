-module(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([parse/1, render/1]).



%% Callbacks



-callback new(Data :: term() | undefined) ->
    Ret :: pdu().

-callback data_map() ->
    jiffy_vm:jv_type().

-callback export(PDU :: pdu()) ->
    marvin_helper_type:generic_return(
        OkRet :: term() | undefined,
        ErrorRet :: term()
    ).



%% Types



-type pdu_impl() ::
    marvin_pdu_generic:pdu() |
    marvin_pdu_heartbeat:pdu() |
    marvin_pdu_hello:pdu() |
    marvin_pdu_identify:pdu() |
    marvin_pdu_heartbeat_ack:pdu().
-type pdu() :: ?marvin_pdu(Mod :: atom(), PDU :: pdu_impl()).
-export_type([pdu/0]).

-type op_dispatch() :: ?discord_op_dispatch.
-type op_heartbeat() :: ?discord_op_heartbeat.
-type op_identify() :: ?discord_op_identify.
-type op_status_update() :: ?discord_op_status_update.
-type op_voice_state_update() :: ?discord_op_voice_state_update.
-type op_voice_server_ping() :: ?discord_op_voice_server_ping.
-type op_resume() :: ?discord_op_resume.
-type op_reconnect() :: ?discord_op_reconnect.
-type op_request_guild_members() :: ?discord_op_request_guild_members.
-type op_invalid_session() :: ?discord_op_invalid_session.
-type op_hello() :: ?discord_op_hello.
-type op_heartbeat_ack() :: ?discord_op_heartbeat_ack.
-type op() ::
    op_dispatch() | op_heartbeat() |
    op_identify() | op_status_update() |
    op_voice_state_update() | op_voice_server_ping() |
    op_resume() | op_reconnect() |
    op_request_guild_members() | op_invalid_session() |
    op_hello() | op_heartbeat_ack().
-export_type([
    op_dispatch/0, op_heartbeat/0, op_identify/0,
    op_status_update/0, op_voice_state_update/0,
    op_voice_server_ping/0, op_resume/0,
    op_reconnect/0, op_request_guild_members/0,
    op_invalid_session/0, op_hello/0,
    op_heartbeat_ack/0, op/0
]).

-type seq() :: non_neg_integer().
-export_type([seq/0]).

-type event() :: binary().
-export_type([event/0]).



%% Interface



-spec parse(Binary :: binary()) ->
    marvin_helper_type:generic_return(
        OkRet :: {pdu(), seq() | undefined},
        ErrorRet ::
            {jiffy_error, term()} |
            not_implemented, invalid_op,
            term()
    ).

parse(Binary) ->
    marvin_helper_chain:chain('marvin_pdu:parse', [
        fun decode_json/1,
        fun detect_data_mod/1,
        fun validate/1,
        fun construct_internal/1
    ], Binary).



-spec render(PDU :: pdu()) ->
    marvin_helper_type:generic_return(
        OkRet :: binary(),
        ErrorRet ::
            {validation_errors, jiffy_vm:jv_ret_errorlist()} |
            invalid_op
    ).

render(PDU) ->
    marvin_helper_chain:chain('marvin_pdu:render', [
        fun detect_op_event/1,
        fun export_data/1,
        fun construct_external/1,
        fun validate/1,
        fun encode_json/1
    ], PDU).



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



-spec detect_data_mod(Message :: #{}) ->
    marvin_helper_type:generic_return(
        OkRet :: {Mod :: atom(), Message :: #{}},
        ErrorRet :: invalid_op
    ).

detect_data_mod(Message) when is_map(Message) ->
    Op = maps:get(?discord_key_op, Message, undefined),
    Event = maps:get(?discord_key_event, Message, undefined),
    case detect_data_mod_by_op_event(Op, Event) of
        {ok, Mod} ->
            {ok, {Mod, Message}};
        {error, invalid_op} ->
            {error, invalid_op}
    end.



-spec validate({Mod :: atom(), Message :: #{}}) ->
    marvin_helper_type:generic_return(
        OkRet :: #{},
        ErrorRet :: {validation_errors, jiffy_vm:jv_ret_errorlist()}
    ).

validate({Mod, Message}) ->
    ValidationMap = generic_validation_map(Mod:data_map()),
    case jiffy_vm:validate(ValidationMap, Message) of
        {[], Result} ->
            {ok, {Mod, Result}};
        {Errors, _Result} ->
            {error, {validation_errors, Errors}}
    end.



-spec construct_internal({Mod :: atom(), Message :: #{}}) ->
    marvin_helper_type:generic_return(
        OkRet :: {pdu(), seq() | undefined},
        ErrorRet :: term()
    ).

construct_internal({Mod, Message}) ->
    Data = maps:get(?discord_key_data, Message, undefined),
    Seq = maps:get(?discord_key_seq, Message, undefined),
    {ok, {Mod:new(Data), Seq}}.



-spec detect_op_event(PDU :: pdu()) ->
    marvin_helper_type:generic_return(
        OkRet :: {Op :: op(), Event :: event() | undefined, PDU :: pdu()},
        ErrorRet :: term()
    ).

detect_op_event(?marvin_pdu(Mod, _) = PDU) ->
    case detect_op_event_by_data_mod(Mod) of
        {ok, {Op, Event}} ->
            {ok, {Op, Event, PDU}};
        {error, invalid_mod} ->
            {error, invalid_mod}
    end.



-spec export_data({Op :: op(), Event :: event() | undefined, PDU :: pdu()}) ->
    marvin_helper_type:generic_return(
        OkRet :: {Mod :: atom(), Op :: op(), Event :: event() | undefined, Data :: #{}},
        ErrorRet :: term()
    ).

export_data({Op, Event, ?marvin_pdu(Mod, _) = PDU}) ->
    case Mod:export(PDU) of
        {ok, Data} ->
            {ok, {Mod, Op, Event, Data}};
        {error, Reason} ->
            {error, Reason}
    end.



-spec construct_external({Mod :: atom(), Op :: op(), Event :: event() | undefined, Data :: #{}}) ->
    marvin_helper_type:ok_return(OkRet :: {Mod :: atom(), Message :: #{}}).

construct_external({Mod, Op, undefined, undefined}) ->
    {ok, {Mod, #{?discord_key_op => Op}}};

construct_external({Mod, Op, Event, undefined}) ->
    {ok, {Mod, #{?discord_key_op => Op, ?discord_key_event => Event}}};

construct_external({Mod, Op, undefined, Data}) ->
    {ok, {Mod, #{?discord_key_op => Op, ?discord_key_data => Data}}};

construct_external({Mod, Op, Event, Data}) ->
    {ok, {Mod, #{?discord_key_op => Op, ?discord_key_event => Event, ?discord_key_data => Data}}}.



-spec encode_json({Mod :: atom(), Message :: #{}}) ->
    marvin_helper_type:generic_return(
        OkRet :: binary(),
        ErrorRet :: {jiffy_error, term()}
    ).

encode_json({_Mod, Message}) ->
    try
        Binary = jiffy:encode(Message),
        {ok, Binary}
    catch _Type:Error ->
        {error, {jiffy_error, Error}}
    end.



-spec generic_validation_map(DataMap :: jiffy_vm:jv_type_object()) ->
    jiffy_vm:jv_type_object().

generic_validation_map(DataMap) ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_op, required, jiffy_vm:integer(fun validate_op/3)),
        jiffy_vm:hashfield(?discord_key_data, optional, DataMap),
        jiffy_vm:hashfield(?discord_key_seq, optional, jiffy_vm:integer(fun validate_seq/3)),
        jiffy_vm:hashfield(?discord_key_event, optional, jiffy_vm:string(fun validate_event/3))
    ], fun validate_pdu/3).



-spec ?validator_spec(validate_op).

validate_op(validate, _, Value) ->
    case lists:member(Value, ?discord_ops) of
        true -> {ok, valid};
        false -> {error, <<"Unknown op">>}
    end;

validate_op(fix, _, _) ->
    {error, invalid}.



-spec ?validator_spec(validate_seq).

validate_seq(validate, _, Value) when Value < 0 ->
    {error, <<"Invalid seq">>};

validate_seq(validate, _, _Value) ->
    {ok, valid};

validate_seq(fix, _, _) ->
    {ok, undefined}.



-spec ?validator_spec(validate_event).

validate_event(validate, _, Value) ->
    case lists:member(Value, ?discord_events) of
        true -> {ok, valid};
        false -> {error, <<"Unknown event">>}
    end;

validate_event(fix, _, _) ->
    {ok, undefined}.



-spec ?validator_spec(validate_pdu).

validate_pdu(validate, _, #{
    ?discord_key_op := ?discord_op_dispatch,
    ?discord_key_event := _Event
}) when undefined =/= _Event ->
    {ok, valid};

validate_pdu(validate, _, #{
    ?discord_key_op := ?discord_op_dispatch
}) ->
    {error, <<"Event is undefined for dispatch op">>};

validate_pdu(validate, _, #{
    ?discord_key_op := _Op,
    ?discord_key_event := _Event
}) when undefined =/= _Event ->
    {error, <<"Event is defined for non-dispatch op">>};

validate_pdu(validate, _, _Value) ->
    {ok, valid};

validate_pdu(fix, _, _) ->
    {error, invalid}.



-spec detect_data_mod_by_op_event(Op :: op(), Event :: event()) ->
    marvin_helper_type:generic_return(
        OkRet :: atom(),
        ErrorRet :: not_implemented | invalid_op
    ).

detect_data_mod_by_op_event(?discord_op_dispatch, ?discord_event_ready) -> {ok, marvin_pdu_dispatch_ready};
detect_data_mod_by_op_event(?discord_op_dispatch, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_heartbeat, _) -> {ok, marvin_pdu_heartbeat};
detect_data_mod_by_op_event(?discord_op_identify, _) -> {ok, marvin_pdu_identify};
detect_data_mod_by_op_event(?discord_op_status_update, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_voice_state_update, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_voice_server_ping, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_resume, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_reconnect, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_request_guild_members, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_invalid_session, _) -> {ok, marvin_pdu_generic};
detect_data_mod_by_op_event(?discord_op_hello, _) -> {ok, marvin_pdu_hello};
detect_data_mod_by_op_event(?discord_op_heartbeat_ack, _) -> {ok, marvin_pdu_heartbeat_ack};
detect_data_mod_by_op_event(_, _) -> {error, invalid_op}.



-spec detect_op_event_by_data_mod(Mod :: atom()) ->
    marvin_helper_type:generic_return(
        OkRet :: {Op :: op(), Event :: event() | undefined},
        ErrorRet :: invalid_mod
    ).

detect_op_event_by_data_mod(marvin_pdu_dispatch_ready) -> {ok, {?discord_op_dispatch, ?discord_event_ready}};
detect_op_event_by_data_mod(marvin_pdu_dispatch) -> {ok, {?discord_op_dispatch, undefined}};
detect_op_event_by_data_mod(marvin_pdu_heartbeat) -> {ok, {?discord_op_heartbeat, undefined}};
detect_op_event_by_data_mod(marvin_pdu_identify) -> {ok, {?discord_op_identify, undefined}};
detect_op_event_by_data_mod(marvin_pdu_status_update) -> {ok, {?discord_op_status_update, undefined}};
detect_op_event_by_data_mod(marvin_pdu_voice_state_update) -> {ok, {?discord_op_voice_state_update, undefined}};
detect_op_event_by_data_mod(marvin_pdu_voice_server_ping) -> {ok, {?discord_op_voice_server_ping, undefined}};
detect_op_event_by_data_mod(marvin_pdu_resume) -> {ok, {?discord_op_resume, undefined}};
detect_op_event_by_data_mod(marvin_pdu_reconnect) -> {ok, {?discord_op_reconnect, undefined}};
detect_op_event_by_data_mod(marvin_pdu_request_guild_members) -> {ok, {?discord_op_request_guild_members, undefined}};
detect_op_event_by_data_mod(marvin_pdu_invalid_session) -> {ok, {?discord_op_invalid_session, undefined}};
detect_op_event_by_data_mod(marvin_pdu_hello) -> {ok, {?discord_op_hello, undefined}};
detect_op_event_by_data_mod(marvin_pdu_heartbeat_ack) -> {ok, {?discord_op_heartbeat_ack, undefined}};
detect_op_event_by_data_mod(_) -> {error, invalid_mod}.
