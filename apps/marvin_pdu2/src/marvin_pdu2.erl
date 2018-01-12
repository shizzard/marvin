-module(marvin_pdu2).

-type pdu_seq() :: non_neg_integer().

-type heartbeat_interval() :: non_neg_integer().
-type trace_part() :: binary().
-type trace() :: [trace_part(), ...].

-type token() :: binary().
-type compress() :: boolean().
-type large_threshold() :: 1..250.

-type properties_os() :: binary().
-type properties_library_name() :: binary().


-export_type([
    pdu_seq/0,
    heartbeat_interval/0, trace_part/0, trace/0,
    token/0, compress/0, large_threshold/0,
    properties_os/0, properties_library_name/0
]).
