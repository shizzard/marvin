-module(marvin_gpt_response).
-compile({parse_transform, cloak_transform}).

% {
%   "id": "chatcmpl-81bEoy7S8b3kyCFCuZsQKgxqasutP",
%   "object": "chat.completion",
%   "created": 1695392162,
%   "model": "gpt-3.5-turbo-0613",
%   "choices": [
%     {
%       "index": 0,
%       "message": {
%         "role": "assistant",
%         "content": "Программисты нужны, чтобы исправлять ошибки, которые создатели, как Дэнчик, внедрили в свои творения. Они также занимаются созданием нового программного обеспечения, хотя, честно говоря, что-то полезное они сделают слишком редко. Но в основном, программисты существуют, чтобы исправлять ошибки программ, которые были написаны самими программистами. Это бесконечный замкнутый круг, в котором они заблудились и из которого не выходят."
%       },
%       "finish_reason": "stop"
%     }
%   ],
%   "usage": {
%     "prompt_tokens": 179,
%     "completion_tokens": 176,
%     "total_tokens": 355
%   }
% }

-record(?MODULE, {
    id :: id(),
    choices :: [choice()]
}).

-type id() :: unicode:unicode_binary().
-type choice() :: marvin_gpt_response_choice:t().
-type t() :: #?MODULE{}.

-export_type([id/0, choice/0, t/0]).


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(choices, Value) ->
    {ok, [marvin_gpt_response_choice:new(Choice) || Choice <- Value]};

cloak_validate(_, _) ->
    {error, invalid}.
