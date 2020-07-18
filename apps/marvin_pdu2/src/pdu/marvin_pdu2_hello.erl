-module(marvin_pdu2_hello).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    heartbeat_interval = undefined :: heartbeat_interval(),
    '_trace' = undefined :: trace()
}).

-type heartbeat_interval() :: pos_integer().
-type trace() :: marvin_pdu2:trace().
-type t() :: #?MODULE{}.

-export_type([heartbeat_interval/0, trace/0, t/0]).

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{heartbeat_interval = HeartbeatInterval, '_trace' = Trace}) ->
    #{<<"heartbeat_interval">> => HeartbeatInterval, <<"_trace">> => Trace}.
