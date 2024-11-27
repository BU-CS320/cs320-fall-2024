open My_parser
open Utils

let parse_input = parse

let get_tail lst =
  match lst with
  | [] -> failwith "空列表无法获取尾部"
  | _ :: tail -> tail

let get_head lst =
  match lst with
  | [] -> failwith "空列表无法获取头部"
  | head :: _ -> head

let rec transform_expr (input_expr : sfexpr) : expr =
  match input_expr with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar var -> Var var
  | SBop (operator, lhs, rhs) ->
      Bop (operator, transform_expr lhs, transform_expr rhs)
  | SIf (cond, then_expr, else_expr) ->
      If (transform_expr cond, transform_expr then_expr, transform_expr else_expr)
  | SAssert assert_expr -> Assert (transform_expr assert_expr)
  | SFun { arg = (param, param_type); args = []; body } ->
      Fun (param, param_type, transform_expr body)
  | SFun { arg = (param, param_type); args = remaining_args; body } ->
      let new_body =
        SFun { arg = get_head remaining_args; args = get_tail remaining_args; body }
      in
      Fun (param, param_type, transform_expr new_body)
  | SLet { is_rec; name; args = []; ty; value; body } ->
      Let
        {
          is_rec;
          name;
          ty;
          value = transform_expr value;
          body = transform_expr body;
        }
  | SLet { is_rec; name; args = arg_head :: arg_tail; ty; value; body } ->
      let func_type =
        List.fold_right (fun (_, t) acc -> FunTy (t, acc)) arg_tail ty
      in
      Let
        {
          is_rec;
          name;
          ty = FunTy (snd arg_head, func_type);
          value =
            transform_expr
              (SFun { arg = arg_head; args = arg_tail; body = value });
          body = transform_expr body;
        }
  | SApp (func_expr, arg_expr) ->
      App (transform_expr func_expr, transform_expr arg_expr)

let transform_program (program : prog) : expr =
  let rec nest_bindings bindings =
    match bindings with
    | [] -> Unit
    | { is_rec; name; args; ty; value } :: rest ->
        let processed_value =
          if args = [] then value
          else SFun { arg = get_head args; args = get_tail args; body = value }
        in
        let func_type =
          List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args ty
        in
        Let
          {
            is_rec;
            name;
            ty = if args = [] then ty else func_type;
            value = transform_expr processed_value;
            body = nest_bindings rest;
          }
  in
  nest_bindings program

type environment = (string * ty) list

let find_in_env ctx var =
  try Ok (List.assoc var ctx) with Not_found -> Error (UnknownVar var)

let rec infer_type (env : environment) (expression : expr) : (ty, error) result =
  match expression with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var var -> find_in_env env var
  | Fun (param, param_type, body) ->
      let extended_env = (param, param_type) :: env in
      (match infer_type extended_env body with
       | Ok body_type -> Ok (FunTy (param_type, body_type))
       | Error e -> Error e)
  (* Continue with other cases following the same structural adjustments... *)
