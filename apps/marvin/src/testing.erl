-module(testing).

-export([connect/0]).


connect() ->
    {ok, Conn} = gun:open("gateway.discord.gg", 443, #{transport => ssl, protocols => [http]}),
    {ok, http} = gun:await_up(Conn),
    _WSRef = gun:ws_upgrade(Conn, "/?encoding=json&v=6", [], #{compress => true}),
    receive
        {gun_ws_upgrade, _ConnPid, ok, _Headers} ->
                io:format("Connection upgraded to websocket state~n");
        {gun_response, _ConnPid, _, _, Status, Headers} ->
                exit({ws_upgrade_failed, Status, Headers});
        {gun_error, _ConnPid, _StreamRef, Reason} ->
                exit({ws_upgrade_failed, Reason})
        %% More clauses here as needed.
    after 1000 ->
            exit(timeout)
    end,
    loop(Conn).


loop(Conn) ->
    receive
        {gun_ws, _, {text, Bin}} ->
            case marvin_pdu:parse(Bin) of
                {ok, PDU} ->
                    handle_pdu(PDU, Conn);
                {error, _} ->
                    io:format("Got unknown binary: ~p~n", [Bin])
            end;
        _Any ->
            io:format("Got unknown message: ~p~n", [_Any])
    end,
    loop(Conn).


handle_pdu(PDU, Conn) ->
    PduMod = element(1, PDU),
    io:format("Got ~p PDU: ~p~n", [PduMod, PDU]),
    case PduMod of
        marvin_pdu_impl_hello ->
            start_heartbeat(Conn, marvin_pdu_impl_hello:heartbeat_interval(PDU)),
            send_identify(Conn),
            ok;
        _ ->
            ok
    end.


start_heartbeat(_Conn, _Interval) ->
    ok.


send_identify(Conn) ->
    PDU = marvin_pdu_impl_identify:new(
        marvin_pdu_data_identify:new(
            <<"Mjc5MjMxMDk4MDgzNjA2NTI5.DEKsTg.XCzTMeI99yjHwnunfD__eaziCS8">>,
            <<"linux">>,
            <<"marvin-0.0.1">>,
            true, 250, [0,1]
        )),
    {ok, Identify} = marvin_pdu_impl_identify:render(PDU),
    io:format("Binary data: ~p~n", [Identify]),
    ok = gun:ws_send(Conn, {text, Identify}).
