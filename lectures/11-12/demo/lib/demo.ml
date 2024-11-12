open Utils
include My_parser

let rec type_of ctxt =
  let rec go = function
    | True -> Some BoolTy
    | False -> Some BoolTy
    | Var x -> List.assoc_opt x ctxt
    | Num _ -> Some IntTy
    | Add (e1, e2) -> go_op IntTy e1 e2
    | Sub (e1, e2) -> go_op IntTy e1 e2
    | Mul (e1, e2) -> go_op IntTy e1 e2
    | Eq (e1, e2) -> go_op BoolTy e1 e2
    | Fun (x, ty, e) -> (
      match type_of ((x, ty) :: ctxt) e with
      | Some t -> Some (FunTy (ty, t))
      | _ -> None
    )
    | If (e1, e2, e3) -> (
      match go e1 with
      | Some BoolTy -> (
        match go e2, go e3 with
        | Some t1, Some t2 ->
          if t1 = t2
          then Some t2
          else None
        | _ -> None
      )
      | _ -> None
    )
    | App (e1, e2) -> (
      match go e1, go e2 with
      | Some (FunTy (t1, t2)), Some t3 ->
        if t1 = t3
        then Some t2
        else None
      | _ -> None
    )
    | Let (x, ty, e1, e2) -> (
      match go e1 with
      | Some t1 ->
        if t1 = ty
        then type_of ((x, ty) :: ctxt) e2
        else None
      | _ -> None
    )
    | LetRec (f, x, ty_arg, ty_val, e1, e2) -> (
      match type_of ((f, FunTy (ty_arg, ty_val)) :: (x, ty_arg) :: ctxt) e1 with
      | Some t ->
        if ty_val = t
        then type_of ((f, FunTy (ty_arg, t)) :: ctxt) e2
        else None
      | _ -> None
    )
  and go_op ty e1 e2 =
    match go e1, go e2 with
    | Some IntTy, Some IntTy -> Some ty
    | _ -> None
  in go

let type_of = type_of []

let rec eval env =
  let rec go = function
    | True -> Some (VBool true)
    | False -> Some (VBool false)
    | Var x -> Env.find_opt x env
    | Num n -> Some (VNum n)
    | Fun (x, _, e) -> Some (VClos (x, e, env, None))
    | Add (e1, e2) -> go_op (+) e1 e2
    | Sub (e1, e2) -> go_op (-) e1 e2
    | Mul (e1, e2) -> go_op ( * ) e1 e2
    | Eq (e1, e2) -> (
      match go e1 with
      | Some (VNum m) -> (
        match go e2 with
        | Some (VNum n) -> Some (VBool (m = n))
        | _ -> None
      )
      | _ -> None
    )
    | If (e1, e2, e3) -> (
      match go e1 with
      | Some (VBool true) -> go e2
      | Some (VBool false) -> go e3
      | _ -> None
    )
  | Let (x, _, e1, e2) -> (
    match go e1 with
    | Some v -> eval (Env.add x v env) e2
    | _ -> None
  )
  | App (e1, e2) -> (
    match go e1 with
    | Some (VClos (x, body, fun_env, None)) -> (
      match go e2 with
      | Some v -> eval (Env.add x v fun_env) body
      | _ -> None
    )
    | Some (VClos (x, body, fun_env, Some f)) -> (
      match go e2 with
      | Some v ->
        let env = Env.add f (VClos (x, body, fun_env, Some f)) fun_env in
        let env = Env.add x v env in
        eval env body
      | _ -> None
    )
    | _ -> None
  )
  | LetRec (f, x, _, _, e1, e2) ->
    eval (Env.add f (VClos (x, e1, env, Some f)) env) e2
  and go_op op e1 e2 =
    match go e1 with
    | Some (VNum m) -> (
      match go e2 with
      | Some (VNum n) -> Some (VNum (op m n))
      | _ -> None
    )
    | _ -> None
  in go

let eval = eval Env.empty

let interp str =
  match parse str with
  | Some expr -> (
    match type_of expr with
    | Some _ -> eval expr
    | _ -> None
  )
  | None -> None
