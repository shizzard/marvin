-module(marvin_pdu2_dispatch_ready).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    guilds :: [marvin_pdu2_object_guild_unavailable:t()],
    private_channels :: [marvin_pdu2_object_channel_dm:t()],
    session_id :: marvin_pdu2:session_id(),
    user :: marvin_pdu2_object_user:t(),
    v :: marvin_pdu2:protocol_version(),
    '_trace' :: marvin_pdu2:trace()
}).


-type t() :: #?MODULE{}.
-export_type([t/0]).


%% Interface



cloak_validate(guilds, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_guild_unavailable:new(Item) || Item <- Value]};

cloak_validate(private_channels, Value) when is_list(Value) ->
    {ok, [marvin_pdu2_object_channel_dm:new(Item) || Item <- Value]};

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
        <<"private_channels">> => [marvin_pdu2_object_channel_dm:export(Item) || Item <- PrivateChannels],
        <<"session_id">> => SessionId,
        <<"user">> => marvin_pdu2_object_user:export(User),
        <<"v">> => Version,
        <<"_trace">> => Trace
    }.
