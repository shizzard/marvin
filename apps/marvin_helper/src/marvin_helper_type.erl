-module(marvin_helper_type).


%% Types


% application specs

-type application_start_type_normal() :: normal.
-type application_start_type_takeover() :: {takeover, Node :: node()}.
-type application_start_type_failover() :: {failover, Node :: node()}.
-type application_start_type() :: application_start_type_normal() | application_start_type_takeover() | application_start_type_failover().

-type application_start_return() :: generic_return(OkRet :: pid(), ErrorRet :: term()) | {ok, pid(), term()}.

-export_type([
    application_start_type_normal/0, application_start_type_takeover/0, application_start_type_failover/0, application_start_type/0,
    application_start_return/0
]).


% gen_server specs

-type gen_server_generic_message() :: term().
-type gen_server_generic_from() :: {Pid :: pid(), Ref :: term()}.

-type gen_server_reply_simple(ReplyT, StateT) :: {reply, Reply :: ReplyT, NewState :: StateT}.
-type gen_server_reply_timeout(ReplyT, StateT) :: {reply, Reply :: ReplyT, NewState :: StateT, Timeout :: non_neg_integer()}.
-type gen_server_reply_hibernate(ReplyT, StateT) :: {reply, Reply :: ReplyT, NewState :: StateT, hibernate}.
-type gen_server_reply(ReplyT, StateT) :: gen_server_reply_simple(ReplyT, StateT) | gen_server_reply_timeout(ReplyT, StateT) | gen_server_reply_hibernate(ReplyT, StateT).

-type gen_server_noreply_simple(StateT) :: {noreply, NewState :: StateT}.
-type gen_server_noreply_timeout(StateT) :: {noreply, NewState :: StateT, Timeout :: non_neg_integer()}.
-type gen_server_noreply_hibernate(StateT) :: {noreply, NewState :: StateT, hibernate}.
-type gen_server_noreply(StateT) :: gen_server_noreply_simple(StateT) | gen_server_noreply_timeout(StateT) | gen_server_noreply_hibernate(StateT).

-type gen_server_stop_simple(StateT) :: {stop, Reason :: term(), Reply :: term(), NewState :: StateT}.
-type gen_server_stop_noreply(StateT) :: {stop, Reason :: term(), NewState :: StateT}.
-type gen_server_stop(StateT) :: gen_server_stop_simple(StateT) | gen_server_stop_noreply(StateT).

-type gen_server_return(StateT) :: gen_server_reply(Reply :: term(), StateT) | gen_server_noreply(StateT) | gen_server_stop(StateT).


-export_type([
    gen_server_generic_message/0, gen_server_generic_from/0,
    gen_server_reply_simple/2, gen_server_reply_timeout/2, gen_server_reply_hibernate/2, gen_server_reply/2,
    gen_server_noreply_simple/1, gen_server_noreply_timeout/1, gen_server_noreply_hibernate/1, gen_server_noreply/1,
    gen_server_stop_simple/1, gen_server_stop_noreply/1, gen_server_stop/1,
    gen_server_return/1
]).


% gen_fsm specs

-type gen_fsm_generic_event() :: term().
-type gen_fsm_generic_from() :: {Pid :: pid(), Ref :: term()}.

-type gen_fsm_noreply_simple(StateT) :: {next_state, NextStateName :: atom(), NewStateData :: StateT}.
-type gen_fsm_noreply_timeout(StateT) :: {next_state, NextStateName :: atom(), NewStateData :: StateT, Timeout :: non_neg_integer()}.
-type gen_fsm_noreply_hibernate(StateT) :: {next_state, NextStateName :: atom(), NewStateData :: StateT, hibernate}.
-type gen_fsm_noreply(StateT) :: gen_fsm_noreply_simple(StateT) | gen_fsm_noreply_timeout(StateT) | gen_fsm_noreply_hibernate(StateT).

-type gen_fsm_reply_simple(StateT) :: {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: StateT}.
-type gen_fsm_reply_timeout(StateT) :: {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: StateT, Timout :: non_neg_integer()}.
-type gen_fsm_reply_hibernate(StateT) :: {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: StateT, hibernate}.
-type gen_fsm_reply(StateT) :: gen_fsm_reply_simple(StateT) | gen_fsm_reply_timeout(StateT) | gen_fsm_reply_hibernate(StateT) | gen_fsm_noreply(StateT).

-type gen_fsm_stop_noreply(StateT) :: {stop, Reason :: term(), NewStateData :: StateT}.
-type gen_fsm_stop_reply(StateT) :: {stop, Reason :: term(), Reply :: term(), NewStateData :: StateT}.
-type gen_fsm_stop(StateT) :: gen_fsm_stop_noreply(StateT) | gen_fsm_stop_reply(StateT).

-type gen_fsm_return_reply(StateT) :: gen_fsm_reply(StateT) | gen_fsm_stop(StateT).
-type gen_fsm_return_noreply(StateT) :: gen_fsm_noreply(StateT) | gen_fsm_stop(StateT).
-type gen_fsm_return(StateT) :: gen_fsm_return_reply(StateT) | gen_fsm_return_noreply(StateT).


-export_type([
    gen_fsm_generic_event/0, gen_fsm_generic_from/0,
    gen_fsm_noreply_simple/1, gen_fsm_noreply_timeout/1, gen_fsm_noreply_hibernate/1, gen_fsm_noreply/1,
    gen_fsm_reply_simple/1, gen_fsm_reply_timeout/1, gen_fsm_reply_hibernate/1, gen_fsm_reply/1,
    gen_fsm_stop_noreply/1, gen_fsm_stop_reply/1, gen_fsm_stop/1,
    gen_fsm_return_reply/1, gen_fsm_return_noreply/1, gen_fsm_return/1
]).


% supervisor specs

-type supervisor_child_spec() :: {
    Id :: term(),
    StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
    Restart :: permanent | transient | temporary,
    Shutdown :: brutal_kill | non_neg_integer(),
    Type :: worker | supervisor,
    Modules :: [atom()] | dynamic
}.
-type supervisor_spec() :: {{
        RestartStrategy :: one_for_all | one_for_one | rest_for_one | simple_one_for_one,
        MaxR :: non_neg_integer(),
        MaxT :: pos_integer()
    }, ChildSpecs :: [supervisor_child_spec()]
}.

-export_type([supervisor_child_spec/0, supervisor_spec/0]).


% generic data structures

-type proplist(KeyT, ValueT) :: [{Key :: KeyT, Value :: ValueT}].

-export_type([proplist/2]).


% generic return values

-type ok_return() :: ok.
-type ok_return(RetT) :: {ok, Ret :: RetT}.
-type ok_return(RetT1, RetT2) :: {ok, Ret1 :: RetT1, Ret2 :: RetT2}.

-type error_return() :: error_return(term()).
-type error_return(ReasonT) :: {error, Reason :: ReasonT}.

-type generic_return() :: ok_return() | error_return().
-type generic_return(ErrorReasonT) :: ok_return() | error_return(ErrorReasonT).
-type generic_return(OkRetT, ErrorReasonT) :: ok_return(OkRetT) | error_return(ErrorReasonT).
-type generic_return(OkRet1, OkRet2, ErrorReasonT) :: ok_return(OkRet1, OkRet2) | error_return(ErrorReasonT).

-export_type([
    ok_return/0, ok_return/1, ok_return/2,
    error_return/0, error_return/1,
    generic_return/0, generic_return/1, generic_return/2, generic_return/3
]).
