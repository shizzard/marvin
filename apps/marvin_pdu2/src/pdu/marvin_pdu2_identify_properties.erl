-module(marvin_pdu2_identify_properties).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    '$os' = undefined :: os(),
    '$browser' = undefined :: browser(),
    '$device' = undefined :: device(),
    '$referrer' = undefined :: referrer(),
    '$referring_domain' = undefined :: referring_domain()
}).

-type os() :: unicode:unicode_binary().
-type browser() :: unicode:unicode_binary().
-type device() :: unicode:unicode_binary().
-type referrer() :: unicode:unicode_binary().
-type referring_domain() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([os/0, browser/0, device/0, referrer/0, referring_domain/0, t/0]).

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    '$os' = OS,
    '$browser' = Browser,
    '$device' = Device,
    '$referrer' = Referrer,
    '$referring_domain' = ReferringDomain
}) ->
    #{
        <<"$os">> => marvin_pdu2:nullify(OS),
        <<"$browser">> => marvin_pdu2:nullify(Browser),
        <<"$device">> => marvin_pdu2:nullify(Device),
        <<"$referrer">> => marvin_pdu2:nullify(Referrer),
        <<"$referring_domain">> => marvin_pdu2:nullify(ReferringDomain)
    }.
