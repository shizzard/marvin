-module(marvin_pdu_dispatch_resumed).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/1, data_map/0, export/1]).
-export([trace/1, trace/2]).



%% Types



-type trace_part() :: binary().
-type trace() :: list(trace_part()).
-export_type([trace_part/0]).

-record(pdu, {
    trace :: trace()
}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_dispatch_resumed(PDU :: pdu_internal()).



%% Interface



-spec new(Data :: #{} | trace()) ->
    Ret :: pdu().

new(#{?discord_key_hello__trace := Trace}) ->
    new(Trace);

new(Trace) ->
    ?marvin_pdu_dispatch_resumed(#pdu{trace = Trace}).



-spec data_map() ->
    jiffy_vm:jv_type_object().

data_map() ->
    jiffy_vm:hash([
        jiffy_vm:hashfield(?discord_key_hello__trace, required, jiffy_vm:list([jiffy_vm:string()]))
    ]).



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: #{}).

export(?marvin_pdu_dispatch_resumed(#pdu{
    trace = Trace
})) ->
    {ok, #{
        ?discord_key_hello__trace => Trace
    }}.



-spec trace(PDU :: pdu()) ->
    Ret :: trace().

trace(?marvin_pdu_dispatch_resumed(#pdu{trace = Value})) ->
    Value.



-spec trace(Trace :: trace(), PDU :: pdu()) ->
    Ret :: pdu().

trace(Value, ?marvin_pdu_dispatch_resumed(#pdu{} = PDU)) ->
    ?marvin_pdu_dispatch_resumed(PDU#pdu{trace = Value}).



%% Internals
