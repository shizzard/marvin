-module(marvin_pdu2_object_embed_video).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% url string  source url of video
% height  integer height of video
% width   integer width of video
-record(?MODULE, {
    url = undefined :: url(),
    height = undefined :: height(),
    width = undefined :: width()
}).

-type url() :: unicode:unicode_binary().
-type height() :: pos_integer().
-type width() :: pos_integer().
-type t() :: #?MODULE{}.

-export_type([url/0, height/0, width/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


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
