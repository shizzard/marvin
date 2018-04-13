-module(marvin_pdu2_object_embed_video).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% url string  source url of video
% height  integer height of video
% width   integer width of video
-record(?MODULE, {
    url :: url(),
    height :: height(),
    width :: width()
}).

-type url() :: unicode:unicode_binary().
-type height() :: pos_integer().
-type width() :: pos_integer().
-type t() :: #?MODULE{}.

-export_type([url/0, height/0, width/0, t/0]).


cloak_validate(url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(height, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(width, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    url = Url,
    height = Height,
    width = Width
}) ->
    #{
        <<"url">> => Url,
        <<"height">> => Height,
        <<"width">> => Width
    }.
