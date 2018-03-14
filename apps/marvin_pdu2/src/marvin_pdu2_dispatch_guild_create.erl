-module(marvin_pdu2_dispatch_guild_create).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: id(),
    name :: name(),
    icon :: icon(),
    splash :: splash(),
    owner :: owner(),
    owner_id :: owner_id(),
    permissions :: permissions(),
    region :: region(),
    afk_channel_id :: afk_channel_id(),
    afk_timeout :: afk_timeout(),
    embed_enabled :: embed_enabled(),
    embed_channel_id :: embed_channel_id(),
    verification_level :: verification_level(),
    default_message_notifications :: default_message_notifications(),
    explicit_content_filter :: explicit_content_filter(),
    roles :: roles(),
    emojis :: emojis(),
    features :: features(),
    mfa_level :: mfa_level(),
    application_id :: application_id(),
    widget_enabled :: widget_enabled(),
    widget_channel_id :: widget_channel_id(),
    system_channel_id :: system_channel_id(),
    joined_at :: joined_at(),
    large :: large(),
    unavailable :: unavailable(),
    member_count :: member_count(),
    voice_states :: voice_states(),
    members :: members(),
    channels :: channels(),
    presences :: presences()
}).

-define(verification_level_none, 0).
-define(verification_level_low, 1).
-define(verification_level_medium, 2).
-define(verification_level_high, 3).
-define(verification_level_veryhigh, 4).

-define(default_message_notifications_all, 0).
-define(default_message_notifications_only_mentions, 1).

-define(explicit_content_filter_disabled, 0).
-define(explicit_content_filter_members_wo_roles, 1).
-define(explicit_content_filter_all, 2).

-define(mfa_level_none, 0).
-define(mfa_level_elevated, 1).

-type id() :: marvin_pdu2:snowflake().
-type name() :: unicode:unicode_binary().
-type icon() :: unicode:unicode_binary().
-type splash() :: unicode:unicode_binary().
-type owner() :: boolean().
-type owner_id() :: marvin_pdu2:snowflake().
-type permissions() :: pos_integer().
-type region() :: unicode:unicode_binary().
-type afk_channel_id() :: marvin_pdu2:snowflake().
-type afk_timeout() :: pos_integer().
-type embed_enabled() :: boolean().
-type embed_channel_id() :: marvin_pdu2:snowflake().
-type verification_level() :: ?verification_level_none..?verification_level_veryhigh.
-type default_message_notifications() :: ?default_message_notifications_all..?default_message_notifications_only_mentions.
-type explicit_content_filter() :: ?explicit_content_filter_disabled..?explicit_content_filter_all.
-type roles() :: [marvin_pdu2_object_role:t()].
-type emojis() :: [marvin_pdu2_object_emoji:t()].
-type features() :: [unicode:unicode_binary()].
-type mfa_level() :: ?mfa_level_none..?mfa_level_elevated.
-type application_id() :: marvin_pdu2:snowflake().
-type widget_enabled() :: boolean().
-type widget_channel_id() :: marvin_pdu2:snowflake().
-type system_channel_id() :: marvin_pdu2:snowflake().
-type joined_at() :: unicode:unicode_binary().
-type large() :: boolean().
-type unavailable() :: false.
-type member_count() :: pos_integer().
-type voice_states() :: [marvin_pdu2_object_voice_state:t()].
-type members() :: [marvin_pdu2_object_member:t()].
-type channels() :: [marvin_pdu2_object_guild_channel:t()].
-type presences() :: [marvin_pdu2_object_presence:t()].
-type t() :: #?MODULE{}.

-export_type([
    id/0,  name/0,  icon/0,  splash/0,  owner/0,  owner_id/0,  permissions/0,
    region/0,  afk_channel_id/0,  afk_timeout/0,  embed_enabled/0,
    embed_channel_id/0,  verification_level/0,  default_message_notifications/0,
    explicit_content_filter/0,  roles/0,  emojis/0,  features/0,  mfa_level/0,
    application_id/0,  widget_enabled/0,  widget_channel_id/0,
    system_channel_id/0,  joined_at/0,  large/0,  unavailable/0,
    member_count/0,  voice_states/0,  members/0,  channels/0,  presences/0, t/0
]).


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(icon, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(splash, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(owner, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(owner_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(permissions, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(region, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(afk_channel_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(afk_timeout, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(embed_enabled, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(embed_channel_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(verification_level, Value)
when is_integer(Value) andalso (
    ?verification_level_none == Value
    orelse ?verification_level_low == Value
    orelse ?verification_level_medium == Value
    orelse ?verification_level_high == Value
    orelse ?verification_level_veryhigh == Value
) ->
    {ok, Value};

cloak_validate(default_message_notifications, Value)
when is_integer(Value) andalso (
    ?default_message_notifications_all == Value
    orelse ?default_message_notifications_only_mentions == Value
) ->
    {ok, Value};

cloak_validate(explicit_content_filter, Value)
when is_integer(Value) andalso (
    ?explicit_content_filter_disabled == Value
    orelse ?explicit_content_filter_members_wo_roles == Value
    orelse ?explicit_content_filter_all == Value
) ->
    {ok, Value};

cloak_validate(roles, Value) when is_list(Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(emojis, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_emoji:new(Item) || Item <- Value]};

cloak_validate(features, Value) when is_list(Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(mfa_level, Value)
when is_integer(Value) andalso (
    ?mfa_level_none == Value
    orelse ?mfa_level_elevated == Value
) ->
    {ok, Value};

cloak_validate(application_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(widget_enabled, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(widget_channel_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(system_channel_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(joined_at, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(large, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(unavailable, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(member_count, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(voice_states, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_voice_state:new(Item) || Item <- Value]};

cloak_validate(members, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_member:new(Item) || Item <- Value]};

cloak_validate(channels, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_guild_channel:new(Item) || Item <- Value]};

cloak_validate(presences, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_presence:new(Item) || Item <- Value]};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    id = Id,
    name = Name,
    icon = Icon,
    splash = Splash,
    owner = Owner,
    owner_id = OwnerId,
    permissions = Permissions,
    region = Region,
    afk_channel_id = AfkChannelId,
    afk_timeout = AfkTimeout,
    embed_enabled = EmbedEnabled,
    embed_channel_id = EmbedChannelId,
    verification_level = VerificationLevel,
    default_message_notifications = DefaultMessageNotifications,
    explicit_content_filter = ExplicitContentFilter,
    roles = Roles,
    emojis = Emojis,
    features = Features,
    mfa_level = MfaLevel,
    application_id = ApplicationId,
    widget_enabled = WidgetEnabled,
    widget_channel_id = WidgetChannelId,
    system_channel_id = SystemChannelId,
    joined_at = JoinedAt,
    large = Large,
    unavailable = Unavailable,
    member_count = MemberCount,
    voice_states = VoiceStates,
    members = Members,
    channels = Channels,
    presences = Presences
}) ->
    #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"icon">> => Icon,
        <<"splash">> => Splash,
        <<"owner">> => Owner,
        <<"owner_id">> => OwnerId,
        <<"permissions">> => Permissions,
        <<"region">> => Region,
        <<"afk_channel_id">> => AfkChannelId,
        <<"afk_timeout">> => AfkTimeout,
        <<"embed_enabled">> => EmbedEnabled,
        <<"embed_channel_id">> => EmbedChannelId,
        <<"verification_level">> => VerificationLevel,
        <<"default_message_notifications">> => DefaultMessageNotifications,
        <<"explicit_content_filter">> => ExplicitContentFilter,
        <<"roles">> => Roles,
        <<"emojis">> => [marvin_pdu2_object_emoji:export(Item) || Item <- Emojis],
        <<"features">> => Features,
        <<"mfa_level">> => MfaLevel,
        <<"application_id">> => ApplicationId,
        <<"widget_enabled">> => WidgetEnabled,
        <<"widget_channel_id">> => WidgetChannelId,
        <<"system_channel_id">> => SystemChannelId,
        <<"joined_at">> => JoinedAt,
        <<"large">> => Large,
        <<"unavailable">> => Unavailable,
        <<"member_count">> => MemberCount,
        <<"voice_states">> => [marvin_pdu2_object_voice_state:export(Item) || Item <- VoiceStates],
        <<"members">> => [marvin_pdu2_object_member:export(Item) || Item <- Members],
        <<"channels">> => [marvin_pdu2_object_guild_channel:export(Item) || Item <- Channels],
        <<"presences">> => [marvin_pdu2_object_presence:export(Item) || Item <- Presences]
    }.
