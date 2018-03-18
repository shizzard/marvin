-module(marvin_config).

-export([get/2, get_integer/2, get_float/2, get_boolean/2]).



%% Interface


-spec get(App :: atom(), Keys :: [atom()] | atom()) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: undefined
    ).

get(App, Keys) ->
    get_impl(application:get_all_env(App), Keys, fun cast_as_is/1).


-spec get_integer(App :: atom(), Keys :: [atom()] | atom()) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: undefined
    ).

get_integer(App, Keys) ->
    get_impl(application:get_all_env(App), Keys, fun cast_as_integer/1).


-spec get_float(App :: atom(), Keys :: [atom()] | atom()) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: undefined
    ).

get_float(App, Keys) ->
    get_impl(application:get_all_env(App), Keys, fun cast_as_float/1).


-spec get_boolean(App :: atom(), Keys :: [atom()] | atom()) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: undefined
    ).

get_boolean(App, Keys) ->
    get_impl(application:get_all_env(App), Keys, fun cast_as_boolean/1).


%% Internals


-spec get_impl(
    Proplist :: marvin_helper_type:proplist(KeyT :: atom(), ValueT :: term()) | undefined,
    Keys :: [atom()] | atom(),
    CastFun :: fun((Term :: term()) -> Ret :: term())
) ->
    marvin_helper_type:generic_return(
        OkRet :: term(),
        ErrorRet :: undefined
    ) | no_return().

get_impl(undefined, _Keys, _CastFun) ->
    {error, undefined};

get_impl(Value, Key, CastFun) when not is_list(Key) ->
    get_impl(Value, [Key], CastFun);

get_impl(Value, [], CastFun) ->
    {ok, CastFun(Value)};

get_impl(Proplist, [Key | Keys], CastFun) when is_list(Proplist) ->
    Struct = proplists:get_value(Key, Proplist),
    get_impl(Struct, Keys, CastFun);

get_impl(_Value, Keys, CastFun) ->
    get_impl(undefined, Keys, CastFun).


-spec cast_as_is(Term :: term()) ->
    Ret :: term().

cast_as_is(Term) ->
    Term.


-spec cast_as_integer(Term :: term()) ->
    Ret :: integer() | no_return().

cast_as_integer(Term) ->
    list_to_integer(Term).



-spec cast_as_float(Term :: term()) ->
    Ret :: float() | no_return().

cast_as_float(Term) ->
    list_to_float(Term).


-spec cast_as_boolean(Term :: term()) ->
    Ret :: boolean() | no_return().

cast_as_boolean("true") ->
    true;

cast_as_boolean("false") ->
    false;

cast_as_boolean(_) ->
    throw(badarg).
