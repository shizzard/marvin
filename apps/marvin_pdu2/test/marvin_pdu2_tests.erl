-module(marvin_pdu2_tests).

-include_lib("eunit/include/eunit.hrl").


can_get_valid_parsed_pdu_test_() ->
    filelib:fold_files(
        code:priv_dir(marvin_pdu2) ++ "/pdu",
        "marvin_pdu2_.*\\.json",
        false,
        fun(File, List) ->
            [fun() ->
                {ok, {PDUMod, TestId}} = detect_pdu_mod(File),
                io:format("~s#~p~n", [PDUMod, TestId]),
                {ok, JSONBin} = file:read_file(File),
                try
                    {ok, Struct} = marvin_pdu2:parse(JSONBin),
                    ?_assertMatch(PDUMod, marvin_pdu2:prot_mod(Struct))
                catch
                    _:_ ->
                        io:format("Stacktrace:~n~p~n", [erlang:get_stacktrace()]),
                        throw(test_failed)
                end
            end | List]
        end,
        []
    ).

can_get_valid_parsed_object_test_() ->
    filelib:fold_files(
        code:priv_dir(marvin_pdu2) ++ "/object",
        "marvin_pdu2_.*\\.json",
        false,
        fun(File, List) ->
            [fun() ->
                {ok, {PDUMod, TestId}} = detect_pdu_mod(File),
                io:format("~s#~p~n", [PDUMod, TestId]),
                try
                    {ok, JSONBin} = file:read_file(File),
                    PDUMod:new(jiffy:decode(JSONBin, [return_maps]))
                catch
                    _:_ ->
                        io:format("Stacktrace:~n~p~n", [erlang:get_stacktrace()]),
                        throw(test_failed)
                end
            end | List]
        end,
        []
    ).

detect_pdu_mod(File) ->
    case re:run(
        filename:basename(File),
        "(?<pdumod>.*)_test_(?<testid>[0-9]+)\\.json",
        [{capture, [pdumod, testid], list}]
    ) of
        {match, [PDUModString, TestIdString]} ->
            {ok, {list_to_atom(PDUModString), list_to_integer(TestIdString)}};
        _ ->
            throw(invalid_filename)
    end.
