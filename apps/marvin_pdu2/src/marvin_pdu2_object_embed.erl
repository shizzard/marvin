-module(marvin_pdu2_object_embed).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% title   string  title of embed
% type    string  type of embed (always "rich" for webhook embeds)
% description string  description of embed
% url string  url of embed
% timestamp   ISO8601 timestamp   timestamp of embed content
% color   integer color code of the embed
% footer  embed footer object footer information
% image   embed image object  image information
% thumbnail   embed thumbnail object  thumbnail information
% video   embed video object  video information
% provider    embed provider object   provider information
% author  embed author object author information
% fields  array of embed field objects    fields information
-record(?MODULE, {
    title = undefined :: title() | undefined,
    type = undefined :: type() | undefined,
    description = undefined :: description() | undefined,
    url = undefined :: url() | undefined,
    timestamp = undefined :: timestamp() | undefined,
    color :: color(),
    footer = undefined :: footer() | undefined,
    image = undefined :: image() | undefined,
    thumbnail = undefined :: thumbnail() | undefined,
    video = undefined :: video() | undefined,
    provider = undefined :: provider() | undefined,
    author = undefined :: author() | undefined,
    fields = [] :: fields()
}).

-type title() :: unicode:unicode_binary().
-type type() :: unicode:unicode_binary(). %% No specs!
-type description() :: unicode:unicode_binary().
-type url() :: unicode:unicode_binary().
-type timestamp() :: unicode:unicode_binary().
-type color() :: 16#000000..16#FFFFFF.
-type footer() :: marvin_pdu2_object_embed_footer:t().
-type image() :: marvin_pdu2_object_embed_image:t().
-type thumbnail() :: marvin_pdu2_object_embed_thumbnail:t().
-type video() :: marvin_pdu2_object_embed_video:t().
-type provider() :: marvin_pdu2_object_embed_provider:t().
-type author() :: marvin_pdu2_object_embed_author:t().
-type fields() :: [marvin_pdu2_object_embed_field:t()].
-type t() :: #?MODULE{}.

-export_type([
    title/0, type/0, description/0, url/0, timestamp/0, color/0, footer/0,
    image/0, thumbnail/0, video/0, provider/0, author/0, fields/0, t/0
]).


cloak_validate(title, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(type, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(description, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(timestamp, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(color, Value) when is_integer(Value) andalso Value >= 16#000000 andalso Value =< 16#FFFFFF ->
    {ok, Value};

cloak_validate(footer, Value) ->
    {ok, marvin_pdu2_object_embed_footer:new(Value)};

cloak_validate(image, Value) ->
    {ok, marvin_pdu2_object_embed_image:new(Value)};

cloak_validate(thumbnail, Value) ->
    {ok, marvin_pdu2_object_embed_thumbnail:new(Value)};

cloak_validate(video, Value) ->
    {ok, marvin_pdu2_object_embed_video:new(Value)};

cloak_validate(provider, Value) ->
    {ok, marvin_pdu2_object_embed_provider:new(Value)};

cloak_validate(author, Value) ->
    {ok, marvin_pdu2_object_embed_author:new(Value)};

cloak_validate(fields, Value) ->
    {ok, [marvin_pdu2_object_embed_field:new(Item) || Item <- Value]};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    title = Title,
    type = Type,
    description = Description,
    url = Url,
    timestamp = Timestamp,
    color = Color,
    footer = Footer,
    image = Image,
    thumbnail = Thumbnail,
    video = Video,
    provider = Provider,
    author = Author,
    fields = Fields
}) ->
    #{
        <<"title">> => marvin_pdu2:nullify(Title),
        <<"type">> => marvin_pdu2:nullify(Type),
        <<"description">> => marvin_pdu2:nullify(Description),
        <<"url">> => marvin_pdu2:nullify(Url),
        <<"timestamp">> => marvin_pdu2:nullify(Timestamp),
        <<"color">> => marvin_pdu2:nullify(Color),
        <<"footer">> => case Footer of
            undefined -> null;
            _ -> marvin_pdu2_object_embed_footer:export(Footer)
        end,
        <<"image">> => case Image of
            undefined -> null;
            _ -> marvin_pdu2_object_embed_image:export(Image)
        end,
        <<"thumbnail">> => case Thumbnail of
            undefined -> null;
            _ -> marvin_pdu2_object_embed_thumbnail:export(Thumbnail)
        end,
        <<"video">> => case Video of
            undefined -> null;
            _ -> marvin_pdu2_object_embed_video:export(Video)
        end,
        <<"provider">> => case Provider of
            undefined -> null;
            _ -> marvin_pdu2_object_embed_provider:export(Provider)
        end,
        <<"author">> => case Author of
            undefined -> null;
            _ -> marvin_pdu2_object_embed_author:export(Author)
        end,
        <<"fields">> => [marvin_pdu2_object_embed_field:export(Item) || Item <- Fields]
    }.
