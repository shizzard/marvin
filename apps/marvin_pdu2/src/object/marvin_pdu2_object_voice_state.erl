-module(marvin_pdu2_object_voice_state).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    user_id = undefined :: user_id(),
    guild_id = undefined :: guild_id() | undefined,
    channel_id = undefined :: channel_id() | undefined,
    session_id = undefined :: session_id(),
    suppress = undefined :: suppress(),
    self_video = undefined :: self_video(),
    self_mute = undefined :: self_mute(),
    self_deaf = undefined :: self_deaf(),
    mute = undefined :: mute(),
    deaf = undefined :: deaf()
}).

-type user_id() :: marvin_pdu2:snowflake().
-type guild_id() :: marvin_pdu2:snowflake().
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
    user_id/0, guild_id/0, channel_id/0, session_id/0, suppress/0, self_video/0,
    self_mute/0, self_deaf/0, mute/0, deaf/0, t/0
]).


cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    user_id = UserId,
    guild_id = GuildId,
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
        <<"user_id">> => marvin_pdu2:nullify(UserId),
        <<"guild_id">> => marvin_pdu2:nullify(GuildId),
        <<"channel_id">> => marvin_pdu2:nullify(ChannelId),
        <<"session_id">> => marvin_pdu2:nullify(SessionId),
        <<"suppress">> => marvin_pdu2:nullify(Suppress),
        <<"self_video">> => marvin_pdu2:nullify(SelfVideo),
        <<"self_mute">> => marvin_pdu2:nullify(SelfMute),
        <<"self_deaf">> => marvin_pdu2:nullify(SelfDeaf),
        <<"mute">> => marvin_pdu2:nullify(Mute),
        <<"deaf">> => marvin_pdu2:nullify(Deaf)
    }.
