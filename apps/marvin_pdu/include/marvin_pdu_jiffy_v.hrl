-define(
    validator_spec(Mod),
    Mod(Type :: validate | fix, Path :: jiffy_vm: jv_ret_stack(), Value :: term()) ->
        marvin_helper_type:generic_return(OkRet :: valid | term(), ErrorRet :: invalid | binary())
).
