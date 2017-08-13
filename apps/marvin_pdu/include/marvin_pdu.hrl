%% Definitions


-define(marvin_pdu(Mod, PDU), {marvin_pdu, Mod, PDU}).

-define(marvin_pdu_generic(PDU), ?marvin_pdu(marvin_pdu_generic, PDU)).

-define(marvin_pdu_dispatch_ready(PDU), ?marvin_pdu(marvin_pdu_dispatch_ready, PDU)).
-define(marvin_pdu_dispatch_resumed(PDU), ?marvin_pdu(marvin_pdu_dispatch_resumed, PDU)).
-define(marvin_pdu_heartbeat(PDU), ?marvin_pdu(marvin_pdu_heartbeat, PDU)).
-define(marvin_pdu_resume(PDU), ?marvin_pdu(marvin_pdu_resume, PDU)).
-define(marvin_pdu_invalid_session(PDU), ?marvin_pdu(marvin_pdu_invalid_session, PDU)).
-define(marvin_pdu_hello(PDU), ?marvin_pdu(marvin_pdu_hello, PDU)).
-define(marvin_pdu_identify(PDU), ?marvin_pdu(marvin_pdu_identify, PDU)).
-define(marvin_pdu_heartbeat_ack(PDU), ?marvin_pdu(marvin_pdu_heartbeat_ack, PDU)).
