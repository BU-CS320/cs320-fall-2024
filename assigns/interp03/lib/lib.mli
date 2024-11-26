open Utils

val parse : string -> prog option

val unify : ty -> constr list -> ty_scheme option

val type_of : stc_env -> expr -> ty_scheme option

val type_check : prog -> ty_scheme option

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

val eval_expr : dyn_env -> expr -> value

val eval : prog -> value

val interp : string -> (value * ty_scheme, error) result
