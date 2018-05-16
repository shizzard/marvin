-module(marvin_dialogflow_response).
-compile({parse_transform, cloak_transform}).

-record(?MODULE, {
    id :: id(),
    result :: result()
}).

-type id() :: unicode:unicode_binary().
-type result() :: marvin_dialogflow_response_result:t().
-type t() :: #?MODULE{}.

-export_type([id/0, result/0, t/0]).


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(result, Value) ->
    {ok, marvin_dialogflow_response_result:new(Value)};

cloak_validate(_, _) ->
    {error, invalid}.
