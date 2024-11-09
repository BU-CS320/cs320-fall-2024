open Utils

(* Substitutes `v` for variable `x` in expression `e` *)
let rec subst v x e =
  match e with
  | Var y -> if y = x then v else e
  | Num _ | Unit | True | False -> e
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) ->
      if y = x then Let (y, subst v x e1, e2)  (* Skip substitution in bound variable *)
      else Let (y, subst v x e1, subst v x e2)
  | Fun (y, e1) ->
      if y = x then e  (* Skip substitution if the variable is bound *)
      else Fun (y, subst v x e1)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)

(* Evaluates expressions and returns a result or an error *)
let rec eval e =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> Error (UnknownVar x)
  | If (e1, e2, e3) ->
      (match eval e1 with
      | Ok (VBool true) -> eval e2
      | Ok (VBool false) -> eval e3
      | _ -> Error InvalidIfCond)
  | Let (x, e1, e2) ->
      (match eval e1 with
      | Ok v -> eval (subst v x e2)
      | Error err -> Error err)
  | Fun (x, e) -> Ok (VFun (x, e))
  | App (e1, e2) ->
      (match eval e1 with
      | Ok (VFun (x, e)) -> (
          match eval e2 with
          | Ok v -> eval (subst v x e)
          | Error err -> Error err)
      | _ -> Error InvalidApp)
  | Bop (op, e1, e2) ->
      let apply_bop op v1 v2 = match op with
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
        | And -> Error (InvalidArgs op) (* `&&` and `||` are Boolean ops *)
        | Or -> Error (InvalidArgs op)
      in
      (match eval e1, eval e2 with
      | Ok (VNum v1), Ok (VNum v2) -> apply_bop op v1 v2
      | Ok (VBool b1), Ok (VBool b2) -> (
          match op with
          | And -> Ok (VBool (b1 && b2))
          | Or -> Ok (VBool (b1 || b2))
          | _ -> Error (InvalidArgs op))
      | _ -> Error (InvalidArgs op))

(* Combines parsing and evaluation *)
let interp s =
  match parse s with
  | Some e -> eval e
  | None -> Error ParseFail
