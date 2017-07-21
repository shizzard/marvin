-spec init(
    Args :: term()
) ->
    {ok, State :: state()} |
    {ok, State :: state(), non_neg_integer() | hibernate} |
    {stop, Reason :: term()} |
    ignore.

-spec handle_call(
    Request :: marvin_helper_type:gen_server_generic_message(),
    From :: marvin_helper_type:gen_server_generic_from(),
    State :: state()
) ->
    marvin_helper_type:gen_server_return(state()).

-spec handle_cast(
    Request :: marvin_helper_type:gen_server_generic_message(),
    State :: state()
) ->
    marvin_helper_type:gen_server_noreply(state()) |
    marvin_helper_type:gen_server_stop_noreply(state()).

-spec handle_info(
    Request :: marvin_helper_type:gen_server_generic_message(),
    State :: state()
) ->
    marvin_helper_type:gen_server_noreply(state()) |
    marvin_helper_type:gen_server_stop_noreply(state()).

-spec terminate(
    Reason :: term(),
    State :: state()
) ->
    marvin_helper_type:ok_return().

-spec code_change(
    OldVsn :: term(),
    State :: state(),
    Extra :: term()
) ->
    marvin_helper_type:ok_return(Ret :: state()).
