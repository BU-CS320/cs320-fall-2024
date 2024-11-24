include Utils

open My_parser

(* Parsing function *)
let parse s =
  try Some (Par.prog Lex.read (Lexing.from_string s))
  with _ -> None

(* Desugaring function *)
let rec desugar (prog: sfexpr): expr =
  match prog with
  | SToplets toplets -> desugar_toplets toplets
  | _ -> failwith "Invalid program structure"

and desugar_toplets (toplets: sftoplet list): expr =
  match toplets with
  | [] -> Unit
  | SToplet { is_rec; name; args; ty; value } :: rest ->
    let func_expr = List.fold_right (fun (arg, ty) acc -> Fun (arg, ty, acc)) args (desugar value) in
    let desugared_rest = desugar_toplets rest in
    if is_rec then
      Let { is_rec = true; name; ty; value = func_expr; body = desugared_rest }
    else
      Let { is_rec = false; name; ty; value = func_expr; body = desugared_rest }

(* Type checking function *)
let rec type_of ctx expr =
  match expr with
  | Num _ -> Ok IntTy
  | True | False -> Ok BoolTy
  | Unit -> Ok UnitTy
  | Var x -> (
      match List.assoc_opt x ctx with
      | Some t -> Ok t
      | None -> Error (UnknownVar x)
    )
  | If (cond, e_then, e_else) -> (
      match type_of ctx cond with
      | Ok BoolTy -> (
          match type_of ctx e_then, type_of ctx e_else with
          | Ok t1, Ok t2 when t1 = t2 -> Ok t1
          | Ok t1, Ok t2 -> Error (IfTyErr (t1, t2))
          | _, Error e -> Error e
        )
      | Ok t -> Error (IfCondTyErr t)
      | Error e -> Error e
    )
  | Fun (x, ty, e_body) ->
      let extended_ctx = (x, ty) :: ctx in
      (match type_of extended_ctx e_body with
      | Ok t_body -> Ok (FunTy (ty, t_body))
      | Error e -> Error e)
  | App (e1, e2) -> (
      match type_of ctx e1, type_of ctx e2 with
      | Ok (FunTy (t_arg, t_ret)), Ok t when t_arg = t -> Ok t_ret
      | Ok (FunTy (t_arg, _)), Ok t -> Error (FunArgTyErr (t_arg, t))
      | Ok t, _ -> Error (FunAppTyErr t)
      | Error e, _ -> Error e
    )
  | Let { is_rec; name; ty; value; body } ->
      let extended_ctx = if is_rec then (name, ty) :: ctx else ctx in
      (match type_of extended_ctx value with
      | Ok t_value when t_value = ty ->
          type_of ((name, ty) :: ctx) body
      | Ok t_value -> Error (LetTyErr (ty, t_value))
      | Error e -> Error e)
  | Assert e -> (
      match type_of ctx e with
      | Ok BoolTy -> Ok UnitTy
      | Ok t -> Error (AssertTyErr t)
      | Error e -> Error e
    )
  | Bop (op, e1, e2) ->
      let check_bop t1 t2 expected_ty =
        match t1, t2 with
        | Ok t1', Ok t2' when t1' = expected_ty && t2' = expected_ty -> Ok expected_ty
        | Ok t1', Ok t2' when t1' <> expected_ty -> Error (OpTyErrL (op, expected_ty, t1'))
        | Ok _, Ok t2' -> Error (OpTyErrR (op, expected_ty, t2'))
        | Error e, _ -> Error e
        | _, Error e -> Error e
      in
      (match op with
      | Add | Sub | Mul | Div | Mod -> check_bop (type_of ctx e1) (type_of ctx e2) IntTy
      | Lt | Lte | Gt | Gte -> check_bop (type_of ctx e1) (type_of ctx e2) IntTy
      | Eq | Neq -> check_bop (type_of ctx e1) (type_of ctx e2) IntTy
      | And | Or -> check_bop (type_of ctx e1) (type_of ctx e2) BoolTy)

(* Evaluation function *)
let rec eval env expr =
  match expr with
  | Num n -> VNum n
  | True -> VBool true
  | False -> VBool false
  | Unit -> VUnit
  | Var x -> (
      match List.assoc_opt x env with
      | Some v -> v
      | None -> raise (UnknownVar x)
    )
  | If (cond, e_then, e_else) -> (
      match eval env cond with
      | VBool true -> eval env e_then
      | VBool false -> eval env e_else
      | _ -> raise InvalidIfCond
    )
  | Fun (x, _, e_body) -> VFun (x, e_body)
  | App (e1, e2) -> (
      match eval env e1 with
      | VFun (x, e_body) ->
          let v_arg = eval env e2 in
          eval ((x, v_arg) :: env) e_body
      | _ -> raise InvalidApp
    )
  | Let { is_rec; name; value; body } ->
      let v = eval env value in
      eval ((name, v) :: env) body
  | Assert e -> (
      match eval env e with
      | VBool true -> VUnit
      | VBool false -> raise AssertFail
      | _ -> raise InvalidIfCond
    )
  | Bop (op, e1, e2) -> eval_binary_op op (eval env e1) (eval env e2)

(* Top-level interpreter *)
let interp s =
  match parse s with
  | None -> Error ParseFail
  | Some sfexpr -> (
      try
        let expr = desugar sfexpr in
        match type_of [] expr with
        | Ok _ -> Ok (eval [] expr)
        | Error e -> Error e
      with
      | DivByZero -> Error DivByZero
      | AssertFail -> Error AssertFail
      | e -> failwith (Printexc.to_string e)
    )
