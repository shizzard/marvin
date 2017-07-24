-module(marvin_helper_time).

-export([timestamp/0]).



%% Interface



-spec timestamp() ->
    Ret :: non_neg_integer().

timestamp() ->
    {M, S, _Ms} = os:timestamp(),
    M * 1000000 + S.
