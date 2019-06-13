-module(marvin_rest2_queue).
-behaviour(gen_server).

-include("marvin_rest2.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([queue_name/1, active_once/2, push_request/2]).



-define(active_once(ExecutorPid), {active_once, ExecutorPid}).
-define(push_request(Req), {push_request, Req}).

-record(state, {
    ratelimit_group :: atom(),
    queue = queue:new() :: queue:queue(),
    executor_queue = queue:new() :: queue:queue()
}).
-type state() :: #state{}.



%% Interface



queue_name(RateLimitGroup) ->
    binary_to_atom(iolist_to_binary([
        atom_to_list(?MODULE), "_", atom_to_list(RateLimitGroup)
    ]), latin1).



active_once(QueuePid, ExecutorPid) ->
    gen_server:call(QueuePid, ?active_once(ExecutorPid)).



push_request(QueuePid, Req) ->
    gen_server:call(QueuePid, ?push_request(Req)).



-spec start_link(RateLimitGroup :: atom()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(RateLimitGroup) ->
    gen_server:start_link({local, queue_name(RateLimitGroup)}, ?MODULE, [RateLimitGroup], []).



init([RateLimitGroup]) ->
    {ok, #state{
        ratelimit_group = RateLimitGroup
    }}.



handle_call(?push_request(Req), _GenReplyTo, S0) ->
    handle_call_push_request(Req, S0);

handle_call(?active_once(ExecutorPid), GenReplyTo, S0) ->
    gen_server:reply(GenReplyTo, ok),
    handle_call_active_once(ExecutorPid, S0);

handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected info", what => handle_info, details => Unexpected}),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



handle_call_push_request(Req, #state{
    ratelimit_group = RateLimitGroup,
    queue = Queue,
    executor_queue = ExecutorQueue
} = S0) ->
    case queue:len(ExecutorQueue) of
        0 ->
            ?l_info(#{
                text => "REST queue placing request into queue, no active executors",
                what => handle_call,
                details => #{
                    ratelimit_group => RateLimitGroup,
                    request => Req
                }
            }),
            {reply, ok, S0#state{
                queue = queue:in(Req, Queue)
            }};
        _ ->
            ?l_debug(#{
                text => "REST queue running request in-place, active executor found",
                what => handle_call,
                details => #{
                    ratelimit_group => RateLimitGroup,
                    request => Req
                }
            }),
            {{value, ExecutorPid}, ExecutorQueue1} = queue:out(ExecutorQueue),
            ok = marvin_rest2_queue_executor:request(ExecutorPid, Req),
            {reply, ok, S0#state{executor_queue = ExecutorQueue1}}
    end.



handle_call_active_once(ExecutorPid, #state{
    ratelimit_group = RateLimitGroup,
    queue = Queue,
    executor_queue = ExecutorQueue
} = S0) ->
    case queue:len(Queue) of
        0 ->
            ?l_info(#{
                text => "REST queue placing executor into queue, no active requests",
                what => handle_call,
                details => #{
                    ratelimit_group => RateLimitGroup
                }
            }),
            {noreply, S0#state{
                executor_queue = queue:in(ExecutorPid, ExecutorQueue)
            }};
        _ ->
            {{value, Req}, Queue1} = queue:out(Queue),
            ?l_info(#{
                text => "REST queue sending active request to the executor",
                what => handle_call,
                details => #{
                    ratelimit_group => RateLimitGroup,
                    request => Req
                }
            }),
            marvin_rest2_queue_executor:request(ExecutorPid, Req),
            {noreply, S0#state{queue = Queue1}}
    end.
