-module(marvin_config).

-export([get/2]).



%% Interface



-spec get(App :: atom(), Keys :: [atom()]) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: undefined
    ).

get(App, Keys) ->
    get_impl(application:get_all_env(App), Keys).



-spec get_impl(
    Proplist :: marvin_helper_type:proplist(KeyT :: atom(), ValueT :: term()) | undefined,
    Keys :: [atom()]
) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: undefined
    ).

get_impl(undefined, _Keys) ->
    {error, undefined};

get_impl(Value, Key) when not is_list(Key) ->
    get_impl(Value, [Key]);

get_impl(Value, []) ->
    {ok, Value};

get_impl(Proplist, [Key | Keys]) when is_list(Proplist) ->
    Struct = proplists:get_value(Key, Proplist),
    get_impl(Struct, Keys);

get_impl(_Value, Keys) ->
    get_impl(undefined, Keys).
