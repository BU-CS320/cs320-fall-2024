open Utils
open My_parser

(* 定义解析错误类型 *)
type parse_error =
  | LexingError of string
  | SyntaxError of string

(* 修改 parse 函数，返回 result 类型 *)
let parse (s : string) : (prog, error) result =
  try
    Ok (Par.prog Lex.read (Lexing.from_string s))
  with
  | Lexing.Error msg -> Error (ParseErr ("Lexing error: " ^ msg))
  | Parsing.Parse_error -> Error (ParseErr "Syntax error")

(* ========== Desugaring ========== *)

let rec desugar_expr (e : sfexpr) : expr =
  match e with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SBop (op, e1, e2) -> Bop (op, desugar_expr e1, desugar_expr e2)
  | SIf (e1, e2, e3) -> If (desugar_expr e1, desugar_expr e2, desugar_expr e3)
  | SAssert e -> Assert (desugar_expr e)
  | SFun { arg = (x, t); args; body } ->
      List.fold_right
        (fun (x, t) acc -> Fun (x, t, acc))
        ((x, t) :: args)
        (desugar_expr body)
  | SLet { is_rec; name; args; ty; value; body } ->
      let value' =
        List.fold_right
          (fun (x, t) acc -> Fun (x, t, acc))
          args
          (desugar_expr value)
      in
      Let { is_rec; name; ty; value = value'; body = desugar_expr body }
  | SApp (e1, e2) -> App (desugar_expr e1, desugar_expr e2)

let desugar (prog : prog) : expr =
  let rec nest_lets defs body =
    match defs with
    | [] -> body
    | { is_rec; name; args; ty; value } :: rest ->
        let value' =
          List.fold_right
            (fun (x, t) acc -> Fun (x, t, acc))
            args
            (desugar_expr value)
        in
        let body' = nest_lets rest body in
        Let { is_rec; name; ty; value = value'; body = body' }
  in
  nest_lets prog Unit

(* ========== Type Checking ========== *)

type context = (string * ty) list

let rec type_of_expr (ctx : context) (e : expr) : (ty, ty_error) result =
  (* ... 保持原有的类型检查实现 ... *)

let type_of (e : expr) : (ty, ty_error) result =
  type_of_expr [] e

(* ========== Evaluation ========== *)

(* 定义求值错误类型 *)
type eval_error =
  | UnboundVariable of string
  | DivisionByZero
  | InvalidIfCondition
  | NonFunctionApplication
  | InvalidOperand
  | InvalidOperator
  | AssertionFailed
  | InvalidAssertion

(* 定义环境类型 *)
type env = (string * value) list

let rec lookup_env x env =
  match env with
  | [] -> Error (UnboundVariable x)
  | (y, v) :: rest -> if x = y then Ok v else lookup_env x rest

let rec eval_expr (env : env) (e : expr) : (value, eval_error) result =
  (* ... 修改后的 eval_expr 实现，见上文 ... *)

let eval (e : expr) : (value, eval_error) result =
  eval_expr [] e

(* ========== Interpreter ========== *)

let interp (s : string) : (value, error) result =
  match parse s with
  | Error e -> Error e
  | Ok prog -> (
      let expr = desugar prog in
      match type_of expr with
      | Error e -> Error (TypeErr e)
      | Ok _ -> (
          match eval expr with
          | Ok v -> Ok v
          | Error e -> Error (EvalErr e)))

(* ========== Error Handling ========== *)

(* 定义错误类型 *)
type error =
  | ParseErr of string
  | TypeErr of ty_error
  | EvalErr of eval_error

(* 定义错误信息函数 *)
let error_msg = function
  | ParseErr msg -> "Parse error: " ^ msg
  | TypeErr err -> (* 您的 type_error_msg 实现 *)
      (* ... *)
      ""
  | EvalErr err -> (
      match err with
      | UnboundVariable x -> "Unbound variable: " ^ x
      | DivisionByZero -> "Division by zero"
      | InvalidIfCondition -> "Invalid condition in if expression"
      | NonFunctionApplication -> "Attempted to apply a non-function value"
      | InvalidOperand -> "Invalid operand"
      | InvalidOperator -> "Invalid operator"
      | AssertionFailed -> "Assertion failed"
      | InvalidAssertion -> "Invalid assertion")
