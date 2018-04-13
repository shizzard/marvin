-module(marvin_pdu2_invalid_session).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    ack = true :: true
}).

-type t() :: #?MODULE{}.
-export_type([t/0]).


export(#?MODULE{}) ->
    #{}.
