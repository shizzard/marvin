-module(marvin_pdu2_dispatch_typing_start).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% channel_id  snowflake   id of the channel
% user_id snowflake   id of the user
% timestamp   integer unix time (in seconds) of when the user started typing
-record(?MODULE, {
    channel_id :: channel_id(),
    user_id :: user_id(),
    timestamp :: timestamp()
}).

-type channel_id() :: marvin_pdu2:showflake().
-type user_id() :: marvin_pdu2:showflake().
-type timestamp() :: non_neg_integer().

-type t() :: #?MODULE{}.

-export_type([channel_id/0, user_id/0, timestamp/0, t/0]).


cloak_validate(channel_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(user_id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(timestamp, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    channel_id = ChannelId,
    user_id = UserId,
    timestamp = Timestamp
}) ->
    #{
        <<"channel_id">> => ChannelId,
        <<"user_id">> => UserId,
        <<"timestamp">> => Timestamp
    }.
