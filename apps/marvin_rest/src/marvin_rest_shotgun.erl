-module(marvin_rest_shotgun).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-record(state, {
    connection :: pid()
}).
-type state() :: #state{}.

-export([
    request/1,
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(request(Req), {request, Req}).



%% Interface



-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



-spec request(Req :: marvin_rest_request:t()) ->
    Ret :: marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: term()
    ).

request(Req) ->
    gen_server:call(?MODULE, ?request(Req)).



init([]) ->
    {ok, ApiHost} = marvin_config:get(marvin, [discord, api, host]),
    {ok, ApiPort} = marvin_config:get_integer(marvin, [discord, api, port]),
    marvin_log:info("Connecting to '~s:~p' REST endpoint", [ApiHost, ApiPort]),
    {ok, Conn} = shotgun:open(ApiHost, ApiPort, https),
    {ok, #state{connection = Conn}}.



%% Internals



handle_call(?request(Req), _GenReplyTo, #state{connection = Conn} = S0) ->
    Method = marvin_rest_request:method(Req),
    Url = marvin_rest_request:url(Req),
    Body = marvin_rest_request:body(Req),
    Headers = [
        {<<"user-agent">>, get_header_user_agent()},
        {<<"authorization">>, get_header_authorization()},
        {<<"content-type">>, <<"application/json">>}
    ],
    marvin_log:info("Request params: Method ~p; Url ~p; Headers ~p; Body ~p", [Method, Url, Headers, Body]),
    {ok, Resp} = shotgun:request(Conn, Method, Url, Headers, Body, #{}),
    {reply, {ok, Resp}, S0};

handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec get_header_user_agent() ->
    Ret :: binary().

get_header_user_agent() ->
    {ok, LibraryName} = marvin_config:get(marvin, [system_info, library_name]),
    {ok, LibraryVersion} = marvin_config:get(marvin, [system_info, library_version]),
    {ok, LibraryWeb} = marvin_config:get(marvin, [system_info, library_web]),
    iolist_to_binary([LibraryName, " (", LibraryWeb, ", v", LibraryVersion, ")"]).



-spec get_header_authorization() ->
    Ret :: binary().

get_header_authorization() ->
    {ok, Token} = marvin_config:get(marvin, [discord, token]),
    iolist_to_binary(["Bot ", binary_to_list(Token)]).

