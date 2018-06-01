-module(marvin_helper_time).

-export([timestamp/0, uptime/0, uptime/1, uptime_string/0]).



%% Interface



-spec timestamp() ->
    Ret :: non_neg_integer().

timestamp() ->
    {M, S, _Ms} = os:timestamp(),
    M * 1000000 + S.



-spec uptime() ->
    Ret :: non_neg_integer().

uptime() ->
    Now = erlang:system_time(),
    StartTime = erlang:time_offset() + erlang:system_info(start_time),
    Now - StartTime.



-spec uptime(Unit :: erlang:time_unit()) ->
    Ret :: non_neg_integer().

uptime(Unit) ->
    erlang:convert_time_unit(uptime(), native, Unit).



-spec uptime_string() ->
    Ret :: unicode:unicode_binary().

uptime_string() ->
    {D, {H, M, S}} = calendar:seconds_to_daystime(uptime(second)),
    list_to_binary(io_lib:format("~3.10.0B:~2.10.0B:~2.10.0B:~2.10.0B", [D, H, M, S])).
