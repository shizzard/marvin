-module(marvin_pdu2_rest_guild_member_update).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    nick :: nick()
}).

-type nick() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([nick/0, t/0]).


cloak_validate(nick, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    nick = Nick
}) ->
    #{
        <<"nick">> => Nick
    }.
