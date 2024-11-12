let rec eval expr =
  match expr with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> Error (UnknownVar x)
  | If (cond, e_then, e_else) -> (
      match eval cond with
      | Ok (VBool true) -> eval e_then
      | Ok (VBool false) -> eval e_else
      | Ok _ -> Error InvalidIfCond
      | Error err -> Error err
    )
  | Let (x, e1, e2) -> (
      match eval e1 with
      | Ok v -> eval (subst v x e2)
      | Error err -> Error err
    )
  | LetRec (f, e1, e2) -> (
      (* For LetRec, substitute f with a recursive function definition *)
      let rec_value = VFun (f, LetRec (f, e1, e1)) in
      eval (subst rec_value f e2)
    )
  | Fun (x, e_body) -> Ok (VFun (x, e_body))
  | App (e1, e2) -> (
      match eval e1 with
      | Ok (VFun (x, e_body)) -> (
          match eval e2 with
          | Ok v -> eval (subst v x e_body)
          | Error err -> Error err
        )
      | Ok _ -> Error InvalidApp
      | Error err -> Error err
    )
  | Bop (op, e1, e2) ->
      let apply_bop op v1 v2 =
        match op with
        | Add -> Ok (VNum (v1 + v2))
        | Sub -> Ok (VNum (v1 - v2))
        | Mul -> Ok (VNum (v1 * v2))
        | Div -> if v2 = 0 then Error DivByZero else Ok (VNum (v1 / v2))
        | Mod -> if v2 = 0 then Error DivByZero else Ok (VNum (v1 mod v2))
        | Lt -> Ok (VBool (v1 < v2))
        | Lte -> Ok (VBool (v1 <= v2))
        | Gt -> Ok (VBool (v1 > v2))
        | Gte -> Ok (VBool (v1 >= v2))
        | Eq -> Ok (VBool (v1 = v2))
        | Neq -> Ok (VBool (v1 <> v2))
        | _ -> Error (InvalidArgs op)
      in
      (match eval e1, eval e2 with
      | Ok (VNum v1), Ok (VNum v2) -> apply_bop op v1 v2
      | Ok (VBool b1), Ok (VBool b2) -> (
          match op with
          | And -> Ok (VBool (b1 && b2))
          | Or -> Ok (VBool (b1 || b2))
          | _ -> Error (InvalidArgs op)
        )
      | _ -> Error (InvalidArgs op))

