-module(marvin_gpt_response_choice).
-compile({parse_transform, cloak_transform}).

% {
%     "index": 0,
%     "message": {
%         "role": "assistant",
%         "content": "Программисты нужны, чтобы исправлять ошибки, которые создатели, как Дэнчик, внедрили в свои творения. Они также занимаются созданием нового программного обеспечения, хотя, честно говоря, что-то полезное они сделают слишком редко. Но в основном, программисты существуют, чтобы исправлять ошибки программ, которые были написаны самими программистами. Это бесконечный замкнутый круг, в котором они заблудились и из которого не выходят."
%     },
%     "finish_reason": "stop"
% }

-record(?MODULE, {
    index :: index(),
    message :: message(),
    finish_reason :: finish_reason()
}).

-type index() :: non_neg_integer().
-type message() :: marvin_gpt_response_choice_message:t().
-type finish_reason() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([index/0, message/0, finish_reason/0, t/0]).


cloak_validate(index, Value) when is_integer(Value) ->
    {ok, Value};

cloak_validate(message, Value) ->
    {ok, marvin_gpt_response_choice_message:new(Value)};

cloak_validate(finish_reason, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.
