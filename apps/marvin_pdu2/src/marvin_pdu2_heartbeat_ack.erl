-module(marvin_pdu2_heartbeat_ack).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {}).


export(#?MODULE{}) ->
    % This PDU is quite inconsistent: no data in it
    undefined.
