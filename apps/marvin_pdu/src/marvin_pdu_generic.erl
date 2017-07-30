-module(marvin_pdu_generic).
-behavior(marvin_pdu).
-include("marvin_discord.hrl").
-include("marvin_pdu.hrl").
-include("marvin_pdu_jiffy_v.hrl").

-export([new/0, new/1, data_map/0, export/1]).



%% Types



-record(pdu, {}).
-type pdu_internal() :: #pdu{}.
-type pdu() :: ?marvin_pdu_generic(PDU :: pdu_internal()).



%% Interface



-spec new() ->
    Ret :: pdu().

new() ->
    new(undefined).



-spec new(Any :: undefined) ->
    Ret :: pdu().

new(_Any) ->
    ?marvin_pdu_generic(#pdu{}).



-spec data_map() ->
    jiffy_vm:jv_type_integer().

data_map() ->
    jiffy_vm:any().



-spec export(PDU :: pdu()) ->
    marvin_helper_type:ok_return(OkRet :: undefined).

export(?marvin_pdu_generic(#pdu{})) ->
    {ok, undefined}.

