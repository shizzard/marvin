-module(marvin_helper_chain).

-include_lib("marvin_log/include/marvin_log.hrl").

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
            ?l_info(#{
                text => "Chain skipped function execution",
                what => skip,
                details => #{
                    chain => Name, 'fun' => length(Funs) + 1, reason => Reason,
                    initial_value => InitialValue
                }
            }),
            {skip, Reason};
        {error, Reason} ->
            ?l_error(#{
                text => "Chain dropped function execution with error",
                what => error,
                details => #{
                    chain => Name, 'fun' => length(Funs) + 1, reason => Reason,
                    initial_value => InitialValue
                }
            }),
            {error, Reason}
    catch
        _Type:Reason:Stacktrace ->
            ?l_error(#{
                text => "Chain dropped function execution with crash",
                what => error,
                details => #{
                    chain => Name, 'fun' => length(Funs) + 1, reason => Reason,
                    initial_value => InitialValue, stacktrace => Stacktrace
                }
            }),
            {error, Reason}
    end.



-spec unwrap2(In :: {First :: any(), Item :: any()}) ->
    Ret :: any().

unwrap2({_, Item}) ->
    Item;

unwrap2(_) ->
    throw(badarg).
