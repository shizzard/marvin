-module(marvin_pdu2_identify_properties).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    '$os' :: marvin_pdu2:properties_os(),
    library_name :: marvin_pdu2:properties_library_name()
}).

-type t() :: #?MODULE{}.
-export_type([t/0]).



cloak_validate('$os', Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(library_name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    '$os' = OS,
    library_name = LibraryName
}) ->
    #{
        <<"$os">> => OS,
        <<"$browser">> => LibraryName,
        <<"$device">> => LibraryName,
        <<"$referrer">> => <<>>,
        <<"$referring_domain">> => <<>>
    }.
