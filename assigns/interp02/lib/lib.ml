(* lib.ml *)

open Utils

(* ========== Parsing ========== *)

let parse (input : string) : prog option =
  try
    Some (Par.prog Lex.read (Lexing.from_string input))
  with
  | _ -> None

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
  match e with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x ->
      (match List.assoc_opt x ctx with
       | Some ty -> Ok ty
       | None -> Error (UnknownVar x))
  | Fun (x, t1, e) ->
      let ctx' = (x, t1) :: ctx in
      (match type_of_expr ctx' e with
       | Ok t2 -> Ok (FunTy (t1, t2))
       | Error e -> Error e)
  | App (e1, e2) ->
      (match type_of_expr ctx e1 with
       | Ok (FunTy (t1, t2)) -> (
           match type_of_expr ctx e2 with
           | Ok t when t = t1 -> Ok t2
           | Ok t -> Error (FunArgTyErr (t1, t))
           | Error e -> Error e)
       | Ok ty -> Error (FunAppTyErr ty)
       | Error e -> Error e)
  | Let { is_rec; name; ty; value; body } ->
      let ctx' = if is_rec then (name, ty) :: ctx else ctx in
      (match type_of_expr ctx' value with
       | Ok vty when vty = ty ->
           type_of_expr ((name, ty) :: ctx) body
       | Ok vty -> Error (LetTyErr (ty, vty))
       | Error e -> Error e)
  | If (e1, e2, e3) ->
      (match type_of_expr ctx e1 with
       | Ok BoolTy -> (
           match (type_of_expr ctx e2, type_of_expr ctx e3) with
           | Ok t2, Ok t3 when t2 = t3 -> Ok t2
           | Ok t2, Ok t3 -> Error (IfTyErr (t2, t3))
           | Error e, _ -> Error e
           | _, Error e -> Error e)
       | Ok ty -> Error (IfCondTyErr ty)
       | Error e -> Error e)
  | Bop (op, e1, e2) ->
      (* Handle binary operator type checking *)
      (* ... 您已有的实现 ... *)
      (* 保持不变 *)

let type_of (e : expr) : (ty, ty_error) result =
  type_of_expr [] e

(* ========== Evaluation ========== *)

type env = (string * value) list

let rec lookup_env x env =
  match env with
  | [] -> Error (UnboundVariable x)
  | (y, v) :: rest -> if x = y then Ok v else lookup_env x rest

let rec eval_expr (env : env) (e : expr) : (value, eval_error) result =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> lookup_env x env
  | Fun (x, _, body) -> Ok (VClos { name = None; arg = x; body; env })
  | App (e1, e2) -> (
      match eval_expr env e1 with
      | Ok (VClos { name; arg; body; env = env' }) -> (
          match eval_expr env e2 with
          | Ok v2 ->
              let env'' = (arg, v2) :: (match name with Some f -> (f, VClos { name; arg; body; env = env' }) :: env' | None -> env') in
              eval_expr env'' body
          | Error e -> Error e)
      | Ok _ -> Error NonFunctionApplication
      | Error e -> Error e)
  | Let { is_rec; name; value; body; _ } ->
      if is_rec then
        let rec env' = (name, v) :: env
        and v =
          match value with
          | Fun (x, _, body) -> VClos { name = Some name; arg = x; body; env = env' }
          | _ -> failwith "let rec only supports functions"
        in
        eval_expr env' body
      else
        (match eval_expr env value with
         | Ok v -> eval_expr ((name, v) :: env) body
         | Error e -> Error e)
  | If (e1, e2, e3) -> (
      match eval_expr env e1 with
      | Ok (VBool true) -> eval_expr env e2
      | Ok (VBool false) -> eval_expr env e3
      | Ok _ -> Error InvalidIfCondition
      | Error e -> Error e)
  | Bop (op, e1, e2) ->
      (* Handle binary operation evaluation *)
      (* ... 您已有的实现 ... *)
      (* 修改为返回 Error 而非异常 *)
  | Assert e -> (
      match eval_expr env e with
      | Ok (VBool true) -> Ok VUnit
      | Ok (VBool false) -> Error AssertionFailed
      | Ok _ -> Error InvalidAssertion
      | Error e -> Error e)

let eval (e : expr) : (value, eval_error) result =
  eval_expr [] e

(* ========== Interpreter ========== *)

let interp s : (value, error) result =
  match parse s with
  | None -> Error ParseErr
  | Some prog ->
      let expr = desugar prog in
      match type_of expr with
      | Error e -> Error (TypeErr e)
      | Ok _ -> (
          match eval expr with
          | Ok v -> Ok v
          | Error e -> Error (EvalErr e))
