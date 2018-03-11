-module(marvin_pdu2_identify_properties).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    '$os' :: os(),
    '$browser' :: browser(),
    '$device' :: device(),
    '$referrer' :: referrer(),
    '$referring_domain' :: referring_domain()
}).

-type os() :: unicode:unicode_binary().
-type browser() :: unicode:unicode_binary().
-type device() :: unicode:unicode_binary().
-type referrer() :: unicode:unicode_binary().
-type referring_domain() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([os/0, browser/0, device/0, referrer/0, referring_domain/0, t/0]).



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
