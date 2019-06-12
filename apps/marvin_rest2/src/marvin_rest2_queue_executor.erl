-module(marvin_rest2_queue_executor).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([request/2]).



-define(detect_queue_neighbour(), {detect_queue_neighbour}).
-define(request(Req), {request, Req}).
-define(execute_request(), {execute_request}).

-record(state, {
    queue_pid :: pid(),
    ratelimit_group :: atom(),
    slots = 1 :: non_neg_integer(),
    replenish_after = 1 :: pos_integer(),
    tref :: erlang:reference() | undefined,
    active_request :: term() %% FIXME: type
}).
-type state() :: #state{}.



%% Interface



request(ExecutorPid, Req) ->
    gen_server:call(ExecutorPid, ?request(Req)).



-spec start_link(RateLimitGroup :: atom()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(RateLimitGroup) ->
    gen_server:start_link(?MODULE, [RateLimitGroup], []).



init([RateLimitGroup]) ->
    {ok, #state{
        ratelimit_group = RateLimitGroup,
        queue_pid = whereis(marvin_rest2_queue:queue_name(RateLimitGroup)),
        tref = init_sleep_timer(0)
    }}.



handle_call(?request(Req), GenReplyTo, S0) ->
    gen_server:reply(GenReplyTo, ok),
    {ok, S1} = do_execute_request(Req, S0),
    {noreply, S1};

handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(?execute_request(), S0) ->
    handle_info_execute_request(S0#state{tref = undefined});

handle_info(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected info", what => handle_info, details => Unexpected}),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



handle_info_execute_request(#state{
    active_request = undefined,
    queue_pid = QueuePid
} = S0) ->
    ok = marvin_rest2_queue:active_once(QueuePid, self()),
    {noreply, S0};

handle_info_execute_request(#state{
    active_request = Req
} = S0) ->
    {ok, S1} = do_execute_request(Req, S0),
    {noreply, S1#state{}}.



do_execute_request(Req, #state{ratelimit_group = RateLimitGroup} = S0) ->
    ?l_debug(#{
        text => "Queue executor is running request",
        what => do_execute_request,
        details => #{
            ratelimit_group => RateLimitGroup,
            request => Req
        }
    }),
    {ok, {StatusCode, {ReplenishAfter, Slots}}} = marvin_rest2:request(Req),
    do_execute_request_handle_response(Req, StatusCode, ReplenishAfter, Slots, S0).




do_execute_request_handle_response(Req, 429 = StatusCode, ReplenishAfter, Slots, #state{
    ratelimit_group = RateLimitGroup
} = S0) ->
    %% Got `HTTP 429 Too Many Requests`, effective retry happening
    %% TODO: hardcode of `HTTP 429 Too Many Requests` error code
    ?l_warning(#{
        text => "Executor got `HTTP 429 Too Many Requests` response",
        what => handle_call,
        details => #{
            ratelimit_group => RateLimitGroup,
            request => Req,
            status_code => StatusCode
        }
    }),
    {ok, S0#state{
        active_request = Req,
        replenish_after = ReplenishAfter,
        slots = Slots
    }};

do_execute_request_handle_response(Req, StatusCode, ReplenishAfter, Slots, #state{
    ratelimit_group = RateLimitGroup,
    queue_pid = QueuePid
} = S0) when Slots > 0 ->
    %% Got effective `200 OK` (-ish) code, execution succeed
    %% We also have additional slots to be executed, thus `active_once`
    ?l_debug(#{
        text => "Executor got a response, having additional slots",
        what => handle_call,
        details => #{
            ratelimit_group => RateLimitGroup,
            request => Req,
            status_code => StatusCode,
            slots_available => Slots
        }
    }),
    marvin_rest2_queue:active_once(QueuePid, self()),
    {ok, S0#state{
        active_request = undefined,
        replenish_after = ReplenishAfter,
        slots = Slots
    }};

do_execute_request_handle_response(Req, StatusCode, ReplenishAfter, Slots, #state{
    ratelimit_group = RateLimitGroup
} = S0) ->
    %% Got effective `200 OK` (-ish) code, execution succeed
    %% No active slots available, thus `init_sleep_timer`
    ?l_debug(#{
        text => "Executor got a response, no additional slots",
        what => handle_call,
        details => #{
            ratelimit_group => RateLimitGroup,
            request => Req,
            status_code => StatusCode,
            replenish_in => ReplenishAfter - marvin_helper_time:timestamp()
        }
    }),
    {ok, S0#state{
        active_request = undefined,
        replenish_after = ReplenishAfter,
        slots = Slots,
        tref = init_sleep_timer(ReplenishAfter)
    }}.



init_sleep_timer(ReplenishAfter) ->
    erlang:send_after(calculate_sleep_time(ReplenishAfter), self(), ?execute_request()).



calculate_sleep_time(ReplenishAfter) ->
    %% Calculating time to sleep before next request being executed
    case marvin_helper_time:timestamp() of
        %% In case current timestamp is less then `X-RateLimit-Reset`,
        %% we need to sleep the difference between these;
        %% NB: casting to milliseconds
        %% TODO: see if we need monotonic time here
        TS when TS < ReplenishAfter ->
            (ReplenishAfter - TS) * 1000;
        TS when TS >= ReplenishAfter ->
            0
    end.
