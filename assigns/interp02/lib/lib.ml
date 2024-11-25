(* ===== 导入模块 ===== *)
open Utils
open My_parser

(* ===== 解析器函数 ===== *)
let parse_input = parse

(* ===== 辅助函数 ===== *)
let get_head lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: _ -> h

let get_tail lst =
  match lst with
  | [] -> failwith "empty list"
  | _ :: t -> t

(* ===== 去糖处理 ===== *)
let rec desugar_expr (sf_expr : sfexpr) : expr =
  match sf_expr with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SBop (op, left, right) -> Bop (op, desugar_expr left, desugar_expr right)
  | SIf (cond, then_branch, else_branch) ->
      If (desugar_expr cond, desugar_expr then_branch, desugar_expr else_branch)
  | SAssert e -> Assert (desugar_expr e)
  | SFun { arg = (x, t); args = []; body } -> Fun (x, t, desugar_expr body)
  | SFun { arg = (x, t); args = arg_list; body } ->
      Fun (x, t, desugar_expr (SFun { arg = get_head arg_list; args = get_tail arg_list; body }))
  | SLet { is_rec; name; args = []; ty; value; body } ->
      Let {
        is_rec;
        name;
        ty;
        value = desugar_expr value;
        body = desugar_expr body;
      }
  | SLet { is_rec; name; args = arg :: args_tail; ty; value; body } ->
      let function_type = List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args_tail ty in
      Let {
        is_rec;
        name;
        ty = FunTy (snd arg, function_type);
        value = desugar_expr (SFun { arg; args = args_tail; body = value });
        body = desugar_expr body;
      }
  | SApp (func, arg) -> App (desugar_expr func, desugar_expr arg)

let desugar (program : prog) : expr =
  let rec nest_lets defs =
    match defs with
    | [] -> Unit
    | { is_rec; name; args; ty; value } :: rest ->
        let value_expr =
          if args = [] then value
          else SFun { arg = get_head args; args = get_tail args; body = value }
        in
        let func_type = List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args ty in
        Let {
          is_rec;
          name;
          ty = if args = [] then ty else func_type;
          value = desugar_expr value_expr;
          body = nest_lets rest;
        }
  in
  nest_lets program

(* ===== 类型检查 ===== *)
type type_env = (string * ty) list

let lookup_type env var =
  try Ok (List.assoc var env) with Not_found -> Error (UnknownVar var)

let rec type_of_expr (env : type_env) (expression : expr) : (ty, error) result =
  match expression with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x -> lookup_type env x
  | Fun (x, t1, body) ->
      let env' = (x, t1) :: env in
      (match type_of_expr env' body with
      | Ok t2 -> Ok (FunTy (t1, t2))
      | Error e -> Error e)
  | App (func, arg) -> (
      match type_of_expr env func with
      | Ok (FunTy (t_param, t_ret)) -> (
          match type_of_expr env arg with
          | Ok t_arg when t_arg = t_param -> Ok t_ret
          | Ok t_arg -> Error (FunArgTyErr (t_param, t_arg))
          | Error e -> Error e)
      | Ok t -> Error (FunAppTyErr t)
      | Error e -> Error e)
  | Let { is_rec; name; ty; value; body } ->
      let env' = if is_rec then (name, ty) :: env else env in
      (match type_of_expr env' value with
      | Ok v_ty when v_ty = ty -> type_of_expr ((name, ty) :: env) body
      | Ok v_ty -> Error (LetTyErr (ty, v_ty))
      | Error e -> Error e)
  | If (cond, then_e, else_e) -> (
      match type_of_expr env cond with
      | Ok BoolTy -> (
          match type_of_expr env then_e, type_of_expr env else_e with
          | Ok t1, Ok t2 when t1 = t2 -> Ok t1
          | Ok t1, Ok t2 -> Error (IfTyErr (t1, t2))
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      | Ok t -> Error (IfCondTyErr t)
      | Error e -> Error e)
  | Bop (op, left, right) -> (
      let check_op expected_ty =
        match type_of_expr env left, type_of_expr env right with
        | Ok t1, Ok t2 when t1 = expected_ty && t2 = expected_ty -> Ok expected_ty
        | Ok t1, Ok t2 -> Error (OpTyErrL (op, expected_ty, t1))
        | Error e, _ -> Error e
        | _, Error e -> Error e
      in
      match op with
      | Add | Sub | Mul | Div | Mod -> check_op IntTy
      | And | Or -> check_op BoolTy
      | Lt | Lte | Gt | Gte | Eq | Neq ->
          (match type_of_expr env left, type_of_expr env right with
          | Ok IntTy, Ok IntTy -> Ok BoolTy
          | Ok t1, Ok t2 -> Error (OpTyErrL (op, IntTy, t1))
          | Error e, _ -> Error e
          | _, Error e -> Error e))
  | Assert e -> (
      match type_of_expr env e with
      | Ok BoolTy -> Ok UnitTy
      | Ok t -> Error (AssertTyErr t)
      | Error e -> Error e)

let type_of expression = type_of_expr [] expression

(* ===== 异常定义 ===== *)
exception AssertFail
exception DivByZero

(* ===== 求值器 ===== *)
let eval expr =
  let rec evaluate env expr =
    match expr with
    | Num n -> VNum n
    | True -> VBool true
    | False -> VBool false
    | Unit -> VUnit
    | Var x -> Env.find x env
    | If (cond, then_e, else_e) ->
        (match evaluate env cond with
        | VBool true -> evaluate env then_e
        | VBool false -> evaluate env else_e
        | _ -> failwith "Conditional Error")
    | Fun (x, _, body) ->
        VClos { name = None; arg = x; body; env }
    | Bop (op, left, right) ->
        (match op with
        | And -> eval_and env left right
        | Or -> eval_or env left right
        | _ -> eval_bop op env left right)
    | Let { is_rec; name = n; ty = _; value = e1; body = e2 } ->
        let value_v =
          match is_rec, e1 with
          | true, Fun (x, _, e) ->
              VClos { name = Some n; arg = x; body = e; env }
          | false, _ -> evaluate env e1
          | _ -> failwith "Recursive binding must be a function"
        in
        evaluate (Env.add n value_v env) e2
    | App (func, arg) ->
        let v_func = evaluate env func in
        let v_arg = evaluate env arg in
        (match v_func with
        | VClos { name; arg = param; body; env = closure_env } ->
            let new_env =
              match name with
              | None -> Env.add param v_arg closure_env
              | Some f -> Env.add param v_arg (Env.add f v_func closure_env)
            in
            evaluate new_env body
        | _ -> failwith "Attempted to apply a non-function value")
    | Assert e ->
        (match evaluate env e with
        | VBool true -> VUnit
        | VBool false -> raise AssertFail
        | _ -> failwith "Assertion must be a boolean")

  and eval_and env e1 e2 =
    match evaluate env e1 with
    | VBool false -> VBool false
    | VBool true -> evaluate env e2
    | _ -> failwith "AND operator requires boolean operands"

  and eval_or env e1 e2 =
    match evaluate env e1 with
    | VBool true -> VBool true
    | VBool false -> evaluate env e2
    | _ -> failwith "OR operator requires boolean operands"

  and eval_bop op env e1 e2 =
    let v1 = evaluate env e1 in
    let v2 = evaluate env e2 in
    match op, v1, v2 with
    | Add, VNum n1, VNum n2 -> VNum (n1 + n2)
    | Sub, VNum n1, VNum n2 -> VNum (n1 - n2)
    | Mul, VNum n1, VNum n2 -> VNum (n1 * n2)
    | Div, VNum n1, VNum n2 when n2 <> 0 -> VNum (n1 / n2)
    | Div, _, VNum 0 -> raise DivByZero
    | Mod, VNum n1, VNum n2 when n2 <> 0 -> VNum (n1 mod n2)
    | Mod, _, VNum 0 -> raise DivByZero
    | Lt, VNum n1, VNum n2 -> VBool (n1 < n2)
    | Lte, VNum n1, VNum n2 -> VBool (n1 <= n2)
    | Gt, VNum n1, VNum n2 -> VBool (n1 > n2)
    | Gte, VNum n1, VNum n2 -> VBool (n1 >= n2)
    | Eq, VNum n1, VNum n2 -> VBool (n1 = n2)
    | Neq, VNum n1, VNum n2 -> VBool (n1 <> n2)
    | _, _, _ -> failwith "Invalid operands for binary operation"

  in
  evaluate Env.empty expr

(* ===== 解释器函数 ===== *)
let interp s =
  match parse_input s with
  | None -> Error ParseErr
  | Some prog ->
      let expr = desugar prog in
      match type_of expr with
      | Error e -> Error e
      | Ok _ ->
          try Ok (eval expr)
          with
          | DivByZero -> Error (OpTyErrR (Div, IntTy, IntTy))
          | AssertFail -> Error (AssertTyErr BoolTy)
