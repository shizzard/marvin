%% Definitions


-define(marvin_pdu_object(Type, Object), {marvin_pdu_object, Type, Object}).

-define(marvin_pdu_object_user(Object), ?marvin_pdu_object(marvin_pdu_object_user, Object)).
-define(marvin_pdu_object_channel_dm(Object), ?marvin_pdu_object(marvin_pdu_object_channel_dm, Object)).
-define(marvin_pdu_object_guild_unavailable(Object), ?marvin_pdu_object(marvin_pdu_object_guild_unavailable, Object)).
