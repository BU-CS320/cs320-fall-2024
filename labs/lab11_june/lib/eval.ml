
open Syntax

(* Think of the return `v` of `eval m` as `m ⇓ v` *)
let rec eval (env : env) e =
  match e with
  | Ann (e, _) -> failwith "undefined"
  | Var x -> failwith "undefined"
  | Fun (x, y, e) -> failwith "undefined"
  | App (fn, arg) -> failwith "undefined"
  | Let (x, e1, e2) -> failwith "undefined"
  | Ifte (guard, b_then, b_else) -> failwith "undefined"

  | EInt i -> VInt i
  | EBool b -> VBool b
  | UnaryOp (op, e) -> eval_unary_op op (eval env e)
  | BinaryOp (op, e1, e2) ->
    let e1' = eval env e1 in
    let e2' = eval env e2 in
    eval_binary_op op e1' e2'

and eval_unary_op op m =
  match op, m with
    | Neg, VInt x -> VInt (-x)
    | Not, VBool x -> VBool (not x)
    | _ -> failwith "eval_UnaryOp"

and eval_binary_op op m n =
  match op, m, n with
    | Add, VInt x, VInt y -> VInt (x + y)
    | Sub, VInt x, VInt y -> VInt (x - y)
    | Mul, VInt x, VInt y -> VInt (x * y)
    | Div, VInt x, VInt y -> VInt (x / y)
    | And, VBool x, VBool y -> VBool (x && y)
    | Or, VBool x, VBool y -> VBool (x || y)
    | Lt, VInt x, VInt y -> VBool (x < y)
    | Gt, VInt x, VInt y -> VBool (x > y)
    | Lte, VInt x, VInt y -> VBool (x <= y)
    | Gte, VInt x, VInt y -> VBool (x >= y)
    | Eq, VInt x, VInt y -> VBool (x = y)
    | Neq, VInt x, VInt y -> VBool (x <> y)
    | _ -> failwith "eval_BinaryOp"