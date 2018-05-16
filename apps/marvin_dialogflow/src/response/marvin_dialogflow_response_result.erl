-module(marvin_dialogflow_response_result).
-compile({parse_transform, cloak_transform}).

-record(?MODULE, {
    source :: source(),
    resolvedQuery :: resolvedQuery(),
    action :: action(),
    parameters :: parameters(),
    fulfillment :: fulfillment(),
    score :: score()
}).

-type source() :: unicode:unicode_binary().
-type resolvedQuery() :: unicode:unicode_binary().
-type action() :: unicode:unicode_binary().
-type parameters() :: term().
-type fulfillment() :: unicode:unicode_binary().
-type score() :: float().
-type t() :: #?MODULE{}.

-export_type([source/0, resolvedQuery/0, action/0, parameters/0, fulfillment/0, score/0, t/0]).


cloak_validate(source, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(resolvedQuery, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(action, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(parameters, Value) ->
    {ok, Value};

cloak_validate(fulfillment, #{<<"speech">> := Text}) ->
    {ok, Text};

cloak_validate(score, Value) when is_float(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.
