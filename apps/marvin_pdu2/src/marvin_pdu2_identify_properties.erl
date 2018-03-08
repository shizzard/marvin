-module(marvin_pdu2_identify_properties).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    '$os' :: marvin_pdu2:properties_os(),
    '$browser' :: marvin_pdu2:properties_browser(),
    '$device' :: marvin_pdu2:properties_device(),
    '$referrer' :: marvin_pdu2:properties_referrer(),
    '$referring_domain' :: marvin_pdu2:properties_referring_domain()
}).

-type t() :: #?MODULE{}.
-export_type([t/0]).



cloak_validate('$os', Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate('$browser', Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate('$device', Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate('$referrer', Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate('$referring_domain', Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    '$os' = OS,
    '$browser' = Browser,
    '$device' = Device,
    '$referrer' = Referrer,
    '$referring_domain' = ReferringDomain
}) ->
    #{
        <<"$os">> => OS,
        <<"$browser">> => Browser,
        <<"$device">> => Device,
        <<"$referrer">> => Referrer,
        <<"$referring_domain">> => ReferringDomain
    }.
