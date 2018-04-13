-module(marvin_pdu2_hello).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    heartbeat_interval :: heartbeat_interval(),
    '_trace' :: trace()
}).

-type heartbeat_interval() :: pos_integer().
-type trace() :: marvin_pdu2:trace().
-type t() :: #?MODULE{}.

-export_type([heartbeat_interval/0, trace/0, t/0]).


cloak_validate(heartbeat_interval, Value) when Value > 0 ->
    {ok, Value};

cloak_validate('_trace', Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{heartbeat_interval = HeartbeatInterval, '_trace' = Trace}) ->
    #{<<"heartbeat_interval">> => HeartbeatInterval, <<"_trace">> => Trace}.
