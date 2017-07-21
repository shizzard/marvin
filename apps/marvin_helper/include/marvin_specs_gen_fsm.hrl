-spec init(
    Args :: term()
) ->
    {ok, StateName :: atom(), State :: state()} |
    {ok, StateName :: atom(), State :: state(), non_neg_integer() | hibernate} |
    {stop, Reason :: term()} |
    ignore.

-spec handle_event(
    Event :: marvin_helper_type:gen_fsm_generic_event(),
    StateName :: atom(),
    State :: state()
) ->
    marvin_helper_type:gen_fsm_return_noreply(state()).

-spec handle_info(
    Info :: marvin_helper_type:gen_fsm_generic_event(),
    StateName :: atom(),
    State :: state()
) ->
    marvin_helper_type:gen_fsm_return_noreply(state()).

-spec handle_sync_event(
    Event :: marvin_helper_type:gen_fsm_generic_event(),
    From :: marvin_helper_type:gen_fsm_generic_from(),
    StateName :: atom(),
    State :: state()
) ->
    marvin_helper_type:gen_fsm_return_reply(state()).

-spec terminate(
    Reason :: term(),
    StateName :: atom(),
    State :: state()
) ->
    marvin_helper_type:ok_return().

-spec code_change(
    OldVsn :: term(),
    StateName :: atom(),
    State :: state(),
    Extra :: term()
) ->
    marvin_helper_type:ok_return(StateName :: atom(), State :: state()).
