(* lib.ml *)

include Utils  (* Re-export everything from Utils to make it accessible *)

open My_parser  (* Ensure this imports the parse function correctly *)

(* Re-export the parse function to make it accessible as Lib.parse *)
let parse = parse

(* Helper function to convert a value to an expression *)
let value_to_expr = function
  | VNum n -> Num n
  | VBool true -> True
  | VBool false -> False
  | VUnit -> Unit
  | VFun (x, e) -> Fun (x, e)

(* Substitutes v (of type value) for variable x in expression e *)
let rec subst v x expr =
  let v_expr = match v with
    | VNum n -> Num n
    | VBool b -> if b then True else False
    | VUnit -> Unit
    | VFun (arg, body) -> Fun (arg, body)
  in
  match expr with
  | Var y -> if y = x then v_expr else Var y
  | Num _ | Unit | True | False -> expr
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  
  | Let (y, e1, e2) ->
      if y = x then Let (y, subst v x e1, e2)  (* Stop substitution in e2 if y shadows x *)
      else if List.mem y (free_vars v_expr) then
        let y' = fresh_var y in  (* Rename y to avoid capture *)
        Let (y', subst v x e1, subst v x (rename y y' e2))
      else
        Let (y, subst v x e1, subst v x e2)

  | Fun (y, e_body) ->
      if y = x then Fun (y, e_body)  (* Avoid substitution in function body if variable is shadowed *)
      else if List.mem y (free_vars v_expr) then
        let y' = fresh_var y in  (* Rename y to avoid capture *)
        Fun (y', subst v x (rename y y' e_body))
      else
        Fun (y, subst v x e_body)
  
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)



(* Evaluates expressions and returns a result or an error *)
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



(* Combines parsing and evaluation *)
let interp s =
  match parse s with
  | Some expr -> eval expr
  | None -> Error ParseFail
