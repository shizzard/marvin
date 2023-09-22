-module(marvin_gpt_response_choice_message).
-compile({parse_transform, cloak_transform}).


% "message": {
%     "role": "assistant",
%     "content": "Программисты нужны, чтобы исправлять ошибки, которые создатели, как Дэнчик, внедрили в свои творения. Они также занимаются созданием нового программного обеспечения, хотя, честно говоря, что-то полезное они сделают слишком редко. Но в основном, программисты существуют, чтобы исправлять ошибки программ, которые были написаны самими программистами. Это бесконечный замкнутый круг, в котором они заблудились и из которого не выходят."
% }

-record(?MODULE, {
    role :: role(),
    content :: content()
}).

-type role() :: unicode:unicode_binary().
-type content() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([role/0, content/0, t/0]).


cloak_validate(role, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(content, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.
