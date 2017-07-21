-module(marvin_pdu_hello).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, new/2, data_map/0, export/1]).
-export([heartbeat_interval/1, heartbeat_interval/2, trace/1, trace/2]).



%% Types



-type heartbeat_interval() :: non_neg_integer().
-type trace_part() :: binary().
-type trace() :: list(trace_part()).
-export_type([
    heartbeat_interval/0, trace_part/0
]).

-record(pdu, {
    heartbeat_interval :: heartbeat_interval(),
    trace :: trace()
}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_hello(PDU :: pdu_internal()).



%% Interface



-spec new(Data :: #{}) ->
    Ret :: pdu().

new(#{
    ?discord_key_hello_heartbeat_interval := HeartbeatInterval,
    ?discord_key_hello__trace := Trace
}) ->
    new(HeartbeatInterval, Trace).



-spec new(
    HeartbeatInterval :: heartbeat_interval(),
    Trace :: list(trace_part())
) ->
    Ret :: pdu().

new(HeartbeatInterval, Trace) ->
    ?marvin_pdu_hello(#pdu{
        heartbeat_interval = HeartbeatInterval,
        trace = Trace
    }).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_hello_heartbeat_interval, required, jiffy_vm:integer(fun validate_heartbeat_interval/3)),
        jiffy_vm:hashfield(?discord_key_hello__trace, required, jiffy_vm:list([jiffy_vm:string()]))
    ]).



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_hello(#pdu{
    heartbeat_interval = HeartbeatInterval,
    trace = Trace
})) ->
    {ok, #{
        ?discord_key_hello_heartbeat_interval => HeartbeatInterval,
        ?discord_key_hello__trace => Trace
    }}.



-spec heartbeat_interval(PDU :: pdu()) ->
    Ret :: heartbeat_interval().

heartbeat_interval(?marvin_pdu_hello(#pdu{heartbeat_interval = Value})) ->
    Value.



-spec heartbeat_interval(
    HeartbeatInterval :: heartbeat_interval(),
    PDU :: pdu()
) ->
    Ret :: pdu().

heartbeat_interval(Value, ?marvin_pdu_hello(#pdu{} = PDU)) ->
    ?marvin_pdu_hello(PDU#pdu{heartbeat_interval = Value}).



-spec trace(PDU :: pdu()) ->
    Ret :: trace().

trace(?marvin_pdu_hello(#pdu{trace = Value})) ->
    Value.



-spec trace(Trace :: trace(), PDU :: pdu()) ->
    Ret :: pdu().

trace(Value, ?marvin_pdu_hello(#pdu{} = PDU)) ->
    ?marvin_pdu_hello(PDU#pdu{trace = Value}).



%% Internals



-spec ?validator_spec(validate_heartbeat_interval).

validate_heartbeat_interval(validate, _, Value)
when Value < 0 ->
    {error, <<"Should be positive">>};

validate_heartbeat_interval(validate, _, _Value) ->
    {ok, valid};

validate_heartbeat_interval(fix, _, _) ->
    {error, invalid}.
