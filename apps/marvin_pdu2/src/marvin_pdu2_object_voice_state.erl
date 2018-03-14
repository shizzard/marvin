-module(marvin_pdu2_object_voice_state).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% {
%     "user_id":"157537831718223872",
%     "channel_id":"321356437068578817",
%     "session_id":"73571a5bd3a647efd6a7c92fae522f65",
%     "suppress":false,
%     "self_video":false,
%     "self_mute":false,
%     "self_deaf":false,
%     "mute":false,
%     "deaf":false
% }
-record(?MODULE, {
    user_id :: user_id(),
    channel_id :: channel_id(),
    session_id :: session_id(),
    suppress :: suppress(),
    self_video :: self_video(),
    self_mute :: self_mute(),
    self_deaf :: self_deaf(),
    mute :: mute(),
    deaf :: deaf()
}).

-type user_id() :: marvin_pdu2:snowflake().
-type channel_id() :: marvin_pdu2:snowflake().
-type session_id() :: marvin_pdu2:snowflake().
-type suppress() :: boolean().
-type self_video() :: boolean().
-type self_mute() :: boolean().
-type self_deaf() :: boolean().
-type mute() :: boolean().
-type deaf() :: boolean().
-type t() :: #?MODULE{}.

-export_type([
    user_id/0, channel_id/0, session_id/0, suppress/0, self_video/0,
    self_mute/0, self_deaf/0, mute/0, deaf/0, t/0
]).


cloak_validate(user_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(channel_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(session_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(suppress, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(self_video, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(self_mute, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(self_deaf, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(mute, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(deaf, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    user_id = UserId,
    channel_id = ChannelId,
    session_id = SessionId,
    suppress = Suppress,
    self_video = SelfVideo,
    self_mute = SelfMute,
    self_deaf = SelfDeaf,
    mute = Mute,
    deaf = Deaf
}) ->
    #{
        <<"user_id">> => UserId,
        <<"channel_id">> => ChannelId,
        <<"session_id">> => SessionId,
        <<"suppress">> => Suppress,
        <<"self_video">> => SelfVideo,
        <<"self_mute">> => SelfMute,
        <<"self_deaf">> => SelfDeaf,
        <<"mute">> => Mute,
        <<"deaf">> => Deaf
    }.
