-module(marvin_helper_chain).

-export([chain/3, unwrap2/1]).

-type chain_name() :: atom().
-type chain_fun() :: fun(
    (InitialValue :: term()) ->
        owl_type:generic_return(
            OkRet :: term(),
            ErrorRet :: term()
        ) | no_return()
).


%% Interface


-spec chain(
    Name :: chain_name(),
    Funs :: [chain_fun()],
    InitialValue :: term()
) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: term()
    ).

chain(_Name, [], Value) ->
    {ok, Value};

chain(Name, [Fun | Funs], InitialValue)
when is_function(Fun, 1) ->
    try Fun(InitialValue) of
        {ok, NewValue} ->
            chain(Name, Funs, NewValue);
        {skip, Reason} ->
            marvin_log:info(
                "Chain ~p skipped fun#-~p ~p execution with reason ~p",
                [Name, length(Funs) + 1, Fun, Reason]),
            {skip, Reason};
        {error, Reason} ->
            marvin_log:info(
                "Chain ~p failed fun#-~p ~p execution with reason ~p",
                [Name, length(Funs) + 1, Fun, Reason]),
            {error, Reason}
    catch
        _Type:Reason ->
            marvin_log:error(
                "Chain ~p crashed fun#-~p ~p execution with reason ~p when initial value was ~p~n~p",
                [Name, length(Funs) + 1, Fun, Reason, InitialValue, erlang:get_stacktrace()]),
            {error, Reason}
    end.



-spec unwrap2(In :: {First :: any(), Item :: any()}) ->
    Ret :: any().

unwrap2({_, Item}) ->
    Item;

unwrap2(_) ->
    throw(badarg).
