-module(marvin_pdu2_dispatch_ready).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guilds :: guilds(),
    private_channels :: private_channels(),
    session_id :: session_id(),
    user :: user(),
    v :: protocol_version(),
    '_trace' :: trace()
}).

-type guilds() :: [marvin_pdu2_object_guild_unavailable:t()].
-type private_channels() :: [marvin_pdu2_object_channel:t()].
-type user() :: marvin_pdu2_object_user:t().
-type session_id() :: marvin_pdu2:session_id().
-type protocol_version() :: pos_integer().
-type trace() :: marvin_pdu2:trace().
-type t() :: #?MODULE{}.

-export_type([
    guilds/0, private_channels/0, user/0, session_id/0,
    protocol_version/0, trace/0, t/0
]).


%% Interface



cloak_validate(guilds, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_guild_unavailable:new(Item) || Item <- Value]};

cloak_validate(private_channels, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_channel:new(Item) || Item <- Value]};

cloak_validate(session_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(user, Value) ->
    {ok, marvin_pdu2_object_user:new(Value)};

cloak_validate(v, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate('_trace', Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    guilds = Guilds,
    private_channels = PrivateChannels,
    session_id = SessionId,
    user = User,
    v = Version,
    '_trace' = Trace
}) ->
    #{
        <<"guilds">> => [marvin_pdu2_object_guild_unavailable:export(Item) || Item <- Guilds],
        <<"private_channels">> => [marvin_pdu2_object_channel:export(Item) || Item <- PrivateChannels],
        <<"session_id">> => SessionId,
        <<"user">> => marvin_pdu2_object_user:export(User),
        <<"v">> => Version,
        <<"_trace">> => Trace
    }.
