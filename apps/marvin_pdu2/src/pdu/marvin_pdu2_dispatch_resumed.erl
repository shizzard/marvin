-module(marvin_pdu2_dispatch_resumed).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    '_trace' = undefined :: trace()
}).

-type trace() :: marvin_pdu2:trace().
-type t() :: #?MODULE{}.

-export_type([trace/0, t/0]).

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{'_trace' = Trace}) ->
    #{<<"_trace">> => Trace}.
