(* lib.ml *)

(* 引入必要的模块 *)
open Utils

(* 确保正确引用解析器和词法分析器模块 *)
open Par
open Lex

(* 解析函数 *)
let parse (input : string) : prog option =
  try
    Some (Par.prog Lex.read (Lexing.from_string input))
  with
  | _ -> None

(* 您的 desugar 函数 *)
let rec desugar_sfexpr (e : sfexpr) : expr =
  (* 您的 desugar_sfexpr 实现，保持不变 *)
  match e with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SFun { arg; args; body } ->
      let rec curry args body =
        match args with
        | [] -> desugar_sfexpr body
        | (x, ty) :: xs -> Fun (x, ty, curry xs body)
      in
      curry (arg :: args) body
  | SApp (e1, e2) ->
      App (desugar_sfexpr e1, desugar_sfexpr e2)
  | SLet { is_rec; name; args; ty; value; body } ->
      let value' =
        let rec curry args value =
          match args with
          | [] -> desugar_sfexpr value
          | (x, ty) :: xs -> Fun (x, ty, curry xs value)
        in
        curry args value
      in
      Let { is_rec; name; ty; value = value'; body = desugar_sfexpr body }
  | SIf (e1, e2, e3) ->
      If (desugar_sfexpr e1, desugar_sfexpr e2, desugar_sfexpr e3)
  | SBop (op, e1, e2) ->
      Bop (op, desugar_sfexpr e1, desugar_sfexpr e2)
  | SAssert e ->
      Assert (desugar_sfexpr e)

(* 去糖化顶层定义 *)
let desugar (prog : prog) : expr =
  let rec aux prog acc =
    match prog with
    | [] -> acc
    | hd :: tl ->
        let e = desugar_sfexpr hd in
        aux tl
          (match e with
           | Let { is_rec; name; ty; value; body = Unit } ->
               Let { is_rec; name; ty; value; body = acc }
           | _ -> e)
  in
  aux (List.rev prog) Unit

(* 您的类型检查函数 *)
let rec type_expr (gamma : (string * ty) list) (e : expr) : (ty, ty_error) result =
  (* 您的 type_expr 实现，保持不变 *)
  match e with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x ->
      (match List.assoc_opt x gamma with
       | Some ty -> Ok ty
       | None -> Error (UnknownVar x))
  | Fun (x, ty_x, body) ->
      let gamma' = (x, ty_x) :: gamma in
      (match type_expr gamma' body with
       | Ok ty_body -> Ok (FunTy (ty_x, ty_body))
       | Error err -> Error err)
  | App (e1, e2) ->
      (match type_expr gamma e1 with
       | Ok (FunTy (ty_arg, ty_res)) ->
           (match type_expr gamma e2 with
            | Ok ty_e2 ->
                if ty_e2 = ty_arg then Ok ty_res
                else Error (FunArgTyErr (ty_arg, ty_e2))
            | Error err -> Error err)
       | Ok ty -> Error (FunAppTyErr ty)
       | Error err -> Error err)
  | Let { is_rec; name; ty; value; body } ->
      if is_rec then
        let gamma' = (name, ty) :: gamma in
        (match type_expr gamma' value with
         | Ok ty_value ->
             if ty_value = ty then
               type_expr gamma' body
             else
               Error (LetTyErr (ty, ty_value))
         | Error err -> Error err)
      else
        (match type_expr gamma value with
         | Ok ty_value ->
             if ty_value = ty then
               type_expr ((name, ty) :: gamma) body
             else
               Error (LetTyErr (ty, ty_value))
         | Error err -> Error err)
  | If (e1, e2, e3) ->
      (match type_expr gamma e1 with
       | Ok BoolTy ->
           (match type_expr gamma e2, type_expr gamma e3 with
            | Ok ty2, Ok ty3 ->
                if ty2 = ty3 then Ok ty2
                else Error (IfTyErr (ty2, ty3))
            | Error err, _ -> Error err
            | _, Error err -> Error err)
       | Ok ty_cond -> Error (IfCondTyErr ty_cond)
       | Error err -> Error err)
  | Bop (op, e1, e2) ->
      (* 根据操作符和操作数类型进行类型检查 *)
      (* ... 您的实现 ... *)
      (* 保持不变 *)
      (match op with
       | Add | Sub | Mul | Div | Mod ->
           (match type_expr gamma e1, type_expr gamma e2 with
            | Ok IntTy, Ok IntTy -> Ok IntTy
            | Ok ty_l, Ok IntTy -> Error (OpTyErrL (op, IntTy, ty_l))
            | Ok IntTy, Ok ty_r -> Error (OpTyErrR (op, IntTy, ty_r))
            | Ok ty_l, Ok ty_r -> Error (OpTyErrL (op, IntTy, ty_l))
            | Error err, _ -> Error err
            | _, Error err -> Error err)
       | Lt | Lte | Gt | Gte ->
           (match type_expr gamma e1, type_expr gamma e2 with
            | Ok IntTy, Ok IntTy -> Ok BoolTy
            | Ok ty_l, Ok IntTy -> Error (OpTyErrL (op, IntTy, ty_l))
            | Ok IntTy, Ok ty_r -> Error (OpTyErrR (op, IntTy, ty_r))
            | Ok ty_l, Ok ty_r -> Error (OpTyErrL (op, IntTy, ty_l))
            | Error err, _ -> Error err
            | _, Error err -> Error err)
       | Eq | Neq ->
           (match type_expr gamma e1, type_expr gamma e2 with
            | Ok ty1, Ok ty2 ->
                if ty1 = ty2 then Ok BoolTy
                else Error (OpTyErrL (op, ty1, ty2))
            | Error err, _ -> Error err
            | _, Error err -> Error err)
       | And | Or ->
           (match type_expr gamma e1, type_expr gamma e2 with
            | Ok BoolTy, Ok BoolTy -> Ok BoolTy
            | Ok ty_l, Ok BoolTy -> Error (OpTyErrL (op, BoolTy, ty_l))
            | Ok BoolTy, Ok ty_r -> Error (OpTyErrR (op, BoolTy, ty_r))
            | Ok ty_l, Ok ty_r -> Error (OpTyErrL (op, BoolTy, ty_l))
            | Error err, _ -> Error err
            | _, Error err -> Error err))
  | Assert e ->
      (match type_expr gamma e with
       | Ok BoolTy -> Ok UnitTy
       | Ok ty -> Error (AssertTyErr ty)
       | Error err -> Error err)

(* 错误类型和错误信息函数 *)
type error =
  | ParseErr
  | EvalErr of eval_error
  | TypeErr of ty_error

(* 求值器错误类型 *)
type eval_error =
  | UnknownVar of string
  | InvalidIfCond
  | InvalidApp
  | DivByZero
  | AssertFailErr
  | InvalidArgs of bop
  | (* 其他 eval 错误类型 *)

(* 错误信息函数 *)
let error_msg = function
  | ParseErr -> "Parse error"
  | EvalErr err -> (
      match err with
      | UnknownVar x -> "Unknown variable: " ^ x
      | InvalidIfCond -> "Invalid condition in if expression"
      | InvalidApp -> "Attempted to apply a non-function"
      | DivByZero -> "Division by zero"
      | AssertFailErr -> "Assertion failed"
      | InvalidArgs op -> "Invalid arguments for operator " ^ string_of_bop op)
  | TypeErr err -> (* 您的 type_error_msg 实现，保持不变 *)
      (* 例如： *)
      let rec type_error_msg = function
        | UnknownVar x -> "Unknown variable: " ^ x
        | FunArgTyErr (expected, actual) ->
            "Function argument type error: expected " ^ string_of_ty expected ^ ", got " ^ string_of_ty actual
        | FunAppTyErr ty ->
            "Attempting to apply a non-function of type " ^ string_of_ty ty
        | LetTyErr (expected, actual) ->
            "Let binding type error: expected " ^ string_of_ty expected ^ ", got " ^ string_of_ty actual
        | IfTyErr (ty1, ty2) ->
            "Branches of if expression have different types: " ^ string_of_ty ty1 ^ " and " ^ string_of_ty ty2
        | IfCondTyErr ty ->
            "Condition of if expression is not a boolean, got type " ^ string_of_ty ty
        | OpTyErrL (op, expected, actual) ->
            "Left operand of " ^ string_of_bop op ^ " has type " ^ string_of_ty actual ^ ", expected " ^ string_of_ty expected
        | OpTyErrR (op, expected, actual) ->
            "Right operand of " ^ string_of_bop op ^ " has type " ^ string_of_ty actual ^ ", expected " ^ string_of_ty expected
        | AssertTyErr ty ->
            "Assert expression is not a boolean, got type " ^ string_of_ty ty

(* 定义环境类型 *)
type env = (string * value) list

(* 修改值类型，增加闭包类型 *)
type value =
  | VNum of int
  | VBool of bool
  | VUnit
  | VClos of string * expr * env  (* 函数闭包，包含参数名、函数体和环境 *)

(* 求值函数 *)
let rec eval (env : env) (e : expr) : (value, eval_error) result =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x ->
      (match List.assoc_opt x env with
       | Some v -> Ok v
       | None -> Error (UnknownVar x))
  | If (e1, e2, e3) ->
      (match eval env e1 with
       | Ok (VBool true) -> eval env e2
       | Ok (VBool false) -> eval env e3
       | Ok _ -> Error InvalidIfCond
       | Error err -> Error err)
  | Let { is_rec = false; name; value = e1; body = e2 } ->
      (match eval env e1 with
       | Ok v -> eval ((name, v) :: env) e2
       | Error err -> Error err)
  | Let { is_rec = true; name; value = e1; body = e2 } ->
      let rec env' = (name, v) :: env
      and v =
        match e1 with
        | Fun (x, _, body) -> VClos (x, body, env')
        | _ -> failwith "let rec only supports functions"
      in
      eval env' e2
  | Fun (x, _, body) -> Ok (VClos (x, body, env))
  | App (e1, e2) ->
      (match eval env e1 with
       | Ok (VClos (x, body, env')) ->
           (match eval env e2 with
            | Ok v2 -> eval ((x, v2) :: env') body
            | Error err -> Error err)
       | Ok _ -> Error InvalidApp
       | Error err -> Error err)
  | Bop (op, e1, e2) ->
      (match eval env e1, eval env e2 with
       | Ok (VNum n1), Ok (VNum n2) ->
           (match op with
            | Add -> Ok (VNum (n1 + n2))
            | Sub -> Ok (VNum (n1 - n2))
            | Mul -> Ok (VNum (n1 * n2))
            | Div -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
            | Mod -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
            | Lt -> Ok (VBool (n1 < n2))
            | Lte -> Ok (VBool (n1 <= n2))
            | Gt -> Ok (VBool (n1 > n2))
            | Gte -> Ok (VBool (n1 >= n2))
            | Eq -> Ok (VBool (n1 = n2))
            | Neq -> Ok (VBool (n1 <> n2))
            | _ -> Error (InvalidArgs op))
       | Ok (VBool b1), Ok (VBool b2) ->
           (match op with
            | And -> Ok (VBool (b1 && b2))
            | Or -> Ok (VBool (b1 || b2))
            | Eq -> Ok (VBool (b1 = b2))
            | Neq -> Ok (VBool (b1 <> b2))
            | _ -> Error (InvalidArgs op))
       | _ -> Error (InvalidArgs op))
  | Assert e ->
      (match eval env e with
       | Ok (VBool true) -> Ok VUnit
       | Ok (VBool false) -> Error AssertFailErr
       | Ok _ -> Error InvalidIfCond
       | Error err -> Error err)

(* 解释器函数 *)
let interp (input : string) : (value, error) result =
  match parse input with
  | None -> Error ParseErr
  | Some prog ->
      let expr = desugar prog in
      match type_expr [] expr with
      | Error err -> Error (TypeErr err)
      | Ok _ ->
          match eval [] expr with
          | Ok v -> Ok v
          | Error err -> Error (EvalErr err)
