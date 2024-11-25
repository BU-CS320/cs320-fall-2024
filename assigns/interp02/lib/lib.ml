open Utils
open My_parser

(* Parse string to AST *)
let parse (input : string) : prog option =
  let lexbuf = Lexing.from_string input in
  try Some (My_parser.prog Lexer.token lexbuf)
  with _ -> None

(* Desugar function: converts sfexpr to expr *)
let rec desugar_sfexpr (s : sfexpr) : expr =
  match s with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SFun { arg; args; body } ->
      let rec curry args body =
        match args with
        | [] -> Fun (fst arg, snd arg, desugar_sfexpr body)
        | (x, ty)::xs -> Fun (x, ty, curry xs body)
      in curry (arg :: args) body
  | SApp (f, x) -> App (desugar_sfexpr f, desugar_sfexpr x)
  | SLet { is_rec; name; args; ty; value; body } ->
      let value_expr = desugar_sfexpr value in
      let rec curry args value =
        match args with
        | [] -> value
        | (x, ty)::xs -> Fun (x, ty, curry xs value)
      in
      Let { is_rec; name; ty; value = curry args value_expr; body = desugar_sfexpr body }
  | SIf (cond, then_, else_) ->
      If (desugar_sfexpr cond, desugar_sfexpr then_, desugar_sfexpr else_)
  | SBop (op, lhs, rhs) -> Bop (op, desugar_sfexpr lhs, desugar_sfexpr rhs)
  | SAssert e -> Assert (desugar_sfexpr e)

let desugar (p : prog) : expr =
  List.fold_right
    (fun toplet acc ->
      let { is_rec; name; args; ty; value } = toplet in
      Let { is_rec; name; ty; value = desugar_sfexpr value; body = acc })
    p Unit

(* Type-checking function *)
let rec type_of (ctx : (string * ty) list) (e : expr) : (ty, error) result =
  match e with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x ->
      (try Ok (List.assoc x ctx)
       with Not_found -> Error (UnknownVar x))
  | Bop (op, lhs, rhs) -> (
      match (op, type_of ctx lhs, type_of ctx rhs) with
      | (Add | Sub | Mul | Div | Mod), Ok IntTy, Ok IntTy -> Ok IntTy
      | (Lt | Lte | Gt | Gte | Eq | Neq), Ok IntTy, Ok IntTy -> Ok BoolTy
      | (And | Or), Ok BoolTy, Ok BoolTy -> Ok BoolTy
      | op, Ok t1, Ok t2 when t1 <> t2 -> Error (OpTyErrL (op, t1, t2))
      | op, _, _ -> Error (OpTyErrR (op, IntTy, BoolTy)))
  | If (cond, then_, else_) -> (
      match (type_of ctx cond, type_of ctx then_, type_of ctx else_) with
      | Ok BoolTy, Ok ty1, Ok ty2 when ty1 = ty2 -> Ok ty1
      | Ok BoolTy, Ok ty1, Ok ty2 -> Error (IfTyErr (ty1, ty2))
      | Ok ty, _, _ -> Error (IfCondTyErr ty)
      | Error err, _, _ -> Error err)
  | Fun (arg, ty, body) ->
      let ctx' = (arg, ty) :: ctx in
      (match type_of ctx' body with
       | Ok ret_ty -> Ok (FunTy (ty, ret_ty))
       | Error err -> Error err)
  | App (f, arg) -> (
      match type_of ctx f with
      | Ok (FunTy (arg_ty, ret_ty)) ->
          (match type_of ctx arg with
           | Ok arg_ty' when arg_ty = arg_ty' -> Ok ret_ty
           | Ok arg_ty' -> Error (FunArgTyErr (arg_ty, arg_ty'))
           | Error err -> Error err)
      | Ok ty -> Error (FunAppTyErr ty)
      | Error err -> Error err)
  | Let { is_rec; name; ty; value; body } ->
      let ctx' = (name, ty) :: ctx in
      let value_ty = type_of (if is_rec then ctx' else ctx) value in
      (match (value_ty, type_of ctx' body) with
       | Ok ty1, Ok ty2 when ty1 = ty -> Ok ty2
       | Ok ty1, Ok _ -> Error (LetTyErr (ty, ty1))
       | Error err, _ -> Error err)
  | Assert e -> (
      match type_of ctx e with
      | Ok BoolTy -> Ok UnitTy
      | Ok ty -> Error (AssertTyErr ty)
      | Error err -> Error err)

(* Evaluation function *)
let rec eval (env : (string * value) list) (e : expr) : value =
  match e with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Num n -> VNum n
  | Var x -> List.assoc x env
  | Bop (op, lhs, rhs) -> (
      let lv = eval env lhs and rv = eval env rhs in
      match (op, lv, rv) with
      | (Add, VNum l, VNum r) -> VNum (l + r)
      | (Sub, VNum l, VNum r) -> VNum (l - r)
      | (Mul, VNum l, VNum r) -> VNum (l * r)
      | (Div, VNum l, VNum r) when r <> 0 -> VNum (l / r)
      | (Mod, VNum l, VNum r) when r <> 0 -> VNum (l mod r)
      | (Lt, VNum l, VNum r) -> VBool (l < r)
      | (Lte, VNum l, VNum r) -> VBool (l <= r)
      | (Gt, VNum l, VNum r) -> VBool (l > r)
      | (Gte, VNum l, VNum r) -> VBool (l >= r)
      | (Eq, VNum l, VNum r) -> VBool (l = r)
      | (Neq, VNum l, VNum r) -> VBool (l <> r)
      | (And, VBool l, VBool r) -> VBool (l && r)
      | (Or, VBool l, VBool r) -> VBool (l || r)
      | _ -> failwith "Runtime error in binary operation")
  | If (cond, then_, else_) -> (
      match eval env cond with
      | VBool true -> eval env then_
      | VBool false -> eval env else_
      | _ -> failwith "Condition is not a boolean")
  | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
  | App (f, arg) -> (
      match eval env f with
      | VClos { name = _; arg = param; body; env = env' } ->
          let arg_val = eval env arg in
          eval ((param, arg_val) :: env') body
      | _ -> failwith "Trying to apply a non-function")
  | Let { is_rec; name; ty = _; value; body } ->
      let value_val = if is_rec then VClos { name = Some name; arg = ""; body = value; env } else eval env value in
      eval ((name, value_val) :: env) body
  | Assert e -> (
      match eval env e with
      | VBool true -> VUnit
      | VBool false -> failwith "Assertion failed"
      | _ -> failwith "Assert expression not a boolean")

(* High-level interpreter function *)
let interp (input : string) : (value, error) result =
  match parse input with
  | None -> Error ParseErr
  | Some prog -> (
      let expr = desugar prog in
      match type_of [] expr with
      | Error err -> Error err
      | Ok _ -> Ok (eval [] expr))
