-module(marvin_pdu2_dispatch_resumed).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    '_trace' :: marvin_pdu2:trace()
}).

-type t() :: #?MODULE{}.
-export_type([t/0]).


cloak_validate('_trace', Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end.


export(#?MODULE{'_trace' = Trace}) ->
    #{<<"_trace">> => Trace}.
