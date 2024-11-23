include Utils  (* Assuming Utils defines VNum, VBool, VUnit, VFun, etc. *)
open My_parser  (* Import My_parser to access parse *)

(* Parsing function *)
let parse s =
  try Some (Par.prog Lex.read (Lexing.from_string s))
  with _ -> None

(* Desugaring function *)
let rec desugar prog =
  match prog with
  | Let (x, e1, e2) -> Let (x, desugar e1, desugar e2)
  | Fun (x, e) -> Fun (x, desugar e)
  | App (e1, e2) -> App (desugar e1, desugar e2)
  | If (e1, e2, e3) -> If (desugar e1, desugar e2, desugar e3)
  | Bop (op, e1, e2) -> Bop (op, desugar e1, desugar e2)
  | Var x -> Var x
  | Num n -> Num n
  | Unit -> Unit
  | True -> True
  | False -> False

(* Type checking function *)
let rec type_of ctx expr =
  match expr with
  | Num _ -> Ok TInt
  | True | False -> Ok TBool
  | Unit -> Ok TUnit
  | Var x -> (
      match List.assoc_opt x ctx with
      | Some t -> Ok t
      | None -> Error (UnknownVar x)
    )
  | If (cond, e_then, e_else) -> (
      match type_of ctx cond with
      | Ok TBool -> (
          match type_of ctx e_then, type_of ctx e_else with
          | Ok t1, Ok t2 when t1 = t2 -> Ok t1
          | Ok t1, Ok t2 -> Error (IfTyErr (t1, t2))
          | _, Error e -> Error e
        )
      | Ok t -> Error (IfCondTyErr t)
      | Error e -> Error e
    )
  | Fun (x, e_body) -> (
      match List.assoc_opt x ctx with
      | Some t -> type_of ((x, t) :: ctx) e_body
      | None -> Error (UnknownVar x)
    )
  | App (e1, e2) -> (
      match type_of ctx e1, type_of ctx e2 with
      | Ok (TFun (t1, t2)), Ok t when t1 = t -> Ok t2
      | Ok (TFun (t1, t2)), Ok t -> Error (FunArgTyErr (t1, t))
      | Ok t, _ -> Error (FunAppTyErr t)
      | Error e, _ -> Error e
    )
  | Bop (op, e1, e2) -> (
      match type_of ctx e1, type_of ctx e2 with
      | Ok TInt, Ok TInt -> Ok TInt
      | Ok TInt, Ok t -> Error (OpTyErrR (op, TInt, t))
      | Ok t, Ok TInt -> Error (OpTyErrL (op, TInt, t))
      | _ -> Error (InvalidArgs op)
    )
  | Let (x, e1, e2) -> (
      match type_of ctx e1 with
      | Ok t -> type_of ((x, t) :: ctx) e2
      | Error e -> Error e
    )

(* Evaluation function *)
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
  | Bop (op, e1, e2) -> (
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
        | And | Or -> Error (InvalidArgs op) (* Not used directly *)
      in
      match eval e1, eval e2 with
      | Ok (VNum v1), Ok (VNum v2) -> apply_bop op v1 v2
      | Ok (VBool b1), Ok (VBool b2) -> (
          match op with
          | And -> Ok (VBool (b1 && b2))
          | Or -> Ok (VBool (b1 || b2))
          | _ -> Error (InvalidArgs op)
        )
      | _ -> Error (InvalidArgs op)
    )

(* Interpreter function *)
let interp s =
  match parse s with
  | Some prog -> (
      let expr = desugar prog in
      match type_of [] expr with
      | Ok _ -> eval expr
      | Error e -> Error e
    )
  | None -> Error ParseFail
