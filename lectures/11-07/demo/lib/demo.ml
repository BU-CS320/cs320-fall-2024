open Utils
include My_parser

let rec eval env =
  let rec go = function
    | True -> Some (VBool true)
    | False -> Some (VBool false)
    | Var x -> Env.find_opt x env
    | Num n -> Some (VNum n)
    | Fun (x, e) -> Some (VClos (x, e, env, None))
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
  | Let (x, e1, e2) -> (
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
  | LetRec (f, x, e1, e2) ->
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
  | Some expr -> eval expr
  | None -> None
