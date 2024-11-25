open Utils
open My_parser

let parse = parse

(* ========== Helper Functions ========== *)

let list_hd = function
 | [] -> failwith "empty list"
 | h :: _ -> h

let list_tl = function
 | [] -> failwith "empty list"
 | _ :: t -> t

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
 | SFun { arg = (x, t); args = []; body } -> Fun (x, t, desugar_expr body)
 | SFun { arg = (x, t); args = y :: ys; body } ->
     Fun (x, t, desugar_expr (SFun { arg = y; args = ys; body }))
 | SLet { is_rec; name; args = []; ty; value; body } ->
     Let
       {
         is_rec;
         name;
         ty;
         value = desugar_expr value;
         body = desugar_expr body;
       }
 | SLet { is_rec; name; args = arg :: args; ty; value; body } ->
     let fun_ty = List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args ty in
     Let
       {
         is_rec;
         name;
         ty = FunTy (snd arg, fun_ty);
         value = desugar_expr (SFun { arg; args; body = value });
         body = desugar_expr body;
       }
 | SApp (e1, e2) -> App (desugar_expr e1, desugar_expr e2)

let desugar (prog : prog) : expr =
 let rec nest_lets = function
   | [] -> Unit
   | { is_rec; name; args; ty; value } :: rest ->
       let value' =
         if args = [] then value
         else SFun { arg = list_hd args; args = list_tl args; body = value }
       in
       let fun_ty =
         List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args ty
       in
       Let
         {
           is_rec;
           name;
           ty = if args = [] then ty else fun_ty;
           value = desugar_expr value';
           body = nest_lets rest;
         }
 in
 nest_lets prog

(* ========== Type Checking ========== *)

type context = (string * ty) list

let lookup ctx x =
 try Ok (List.assoc x ctx) with Not_found -> Error (UnknownVar x)

(* Define type_of_expr first *)
let rec type_of_expr (ctx : context) (e : expr) : (ty, error) result =
  match e with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x -> lookup ctx x
  | Fun (x, t1, e) ->
      let ctx' = (x, t1) :: ctx in
      (match type_of_expr ctx' e with
       | Ok t2 -> Ok (FunTy (t1, t2))
       | Error e -> Error e)
  | App (e1, e2) -> (
      match type_of_expr ctx e1 with
      | Error e -> Error e
      | Ok (FunTy (t1, t2)) -> (
          match type_of_expr ctx e2 with
          | Error e -> Error e
          | Ok t2' when t1 = t2' -> Ok t2
          | Ok t2' -> Error (FunArgTyErr (t1, t2')))
      | Ok t -> Error (FunAppTyErr t))
  | Let { is_rec; name; ty; value; body } ->
      let ctx' = if is_rec then (name, ty) :: ctx else ctx in
      (match type_of_expr ctx' value with
       | Error e -> Error e
       | Ok vty when vty = ty -> type_of_expr ((name, ty) :: ctx) body
       | Ok vty -> Error (LetTyErr (ty, vty)))
  | If (e1, e2, e3) -> (
      match type_of_expr ctx e1 with
      | Error e -> Error e
      | Ok BoolTy -> (
          match type_of_expr ctx e2 with
          | Error e -> Error e
          | Ok t2 -> (
              match type_of_expr ctx e3 with
              | Error e -> Error e
              | Ok t3 when t2 = t3 -> Ok t2
              | Ok t3 -> Error (IfTyErr (t2, t3))))
      | Ok t -> Error (IfCondTyErr t))
  | Bop (op, e1, e2) -> (
      match op with
      | Add | Sub | Mul | Div | Mod -> (
          match (type_of_expr ctx e1, type_of_expr ctx e2) with
          | Ok IntTy, Ok IntTy -> Ok IntTy
          | Ok IntTy, Ok t2 when t2 <> IntTy -> Error (OpTyErrR (op, IntTy, t2))
          | Ok t1, _ when t1 <> IntTy -> Error (OpTyErrL (op, IntTy, t1))
          | Error e, _ -> Error e
          | _, Error e -> Error e
          | _, _ -> Error (OpTyErrL (op, IntTy, UnitTy)))
      | And | Or -> (
          match (type_of_expr ctx e1, type_of_expr ctx e2) with
          | Ok BoolTy, Ok BoolTy -> Ok BoolTy
          | Ok BoolTy, Ok t2 when t2 <> BoolTy -> Error (OpTyErrR (op, BoolTy, t2))
          | Ok t1, _ when t1 <> BoolTy -> Error (OpTyErrL (op, BoolTy, t1))
          | Error e, _ -> Error e
          | _, Error e -> Error e
          | _, _ -> Error (OpTyErrL (op, BoolTy, UnitTy)))
      | Lt | Lte | Gt | Gte | Eq | Neq -> (
          match (type_of_expr ctx e1, type_of_expr ctx e2) with
          | Ok IntTy, Ok IntTy -> Ok BoolTy
          | Ok IntTy, Ok t2 when t2 <> IntTy -> Error (OpTyErrR (op, IntTy, t2))
          | Ok t1, _ when t1 <> IntTy -> Error (OpTyErrL (op, IntTy, t1))
          | Error e, _ -> Error e
          | _, Error e -> Error e
          | _, _ -> Error (OpTyErrL (op, IntTy, UnitTy))))
  | Assert e -> (
      match type_of_expr ctx e with
      | Ok BoolTy -> Ok UnitTy
      | Ok t -> Error (AssertTyErr t)
      | Error e -> Error e)

(* Define type_of after type_of_expr *)
let type_of (e : expr) : (ty, error) result =
  type_of_expr [] e


(* ========== Exceptions ========== *)
exception AssertFail
exception DivByZero


(* ========== Eval ========== *)
(*let eval (_ : expr) : value =
  VUnit*)
  let eval expr =
    let rec eval env expr =
      match expr with
      | Num n -> VNum n
      | True -> VBool true
      | False -> VBool false
      | Unit -> VUnit
      | Var x -> Env.find x env
      | If (e1, e2, e3) ->
          (match eval env e1 with
           | VBool true -> eval env e2
           | VBool false -> eval env e3
           | _ -> failwith "Conditional Error")
      | Fun (x, _, body) -> 
          VClos { name = None; arg = x; body; env }
      | Bop (op, e1, e2) -> 
          (match op with
           | And -> eval_and env e1 e2
           | Or -> eval_or env e1 e2
           | _ -> eval_bop op env e1 e2)
      | Let { is_rec; name = n; ty = _ty; value = e1; body = e2 } ->
          let value_v = match is_rec, e1 with
            | true, Fun (x, _, e) -> 
                VClos { name = Some n; arg = x; body = e; env = env }
            | false, _ -> eval env e1
            | _ -> failwith "recursive binding must be function"
          in
          eval (Env.add n value_v env) e2
      | App (e1, e2) ->
          let v1 = eval env e1 in
          let v2 = eval env e2 in
          (match v1 with
           | VClos { name; arg; body; env = cenv } ->
               let new_env = match name with
                 | None -> Env.add arg v2 cenv
                 | Some f -> Env.add arg v2 (Env.add f v1 cenv)
               in eval new_env body
           | _ -> failwith "Attempted to apply a non-function value")
      | Assert e ->
          (match eval env e with
           | VBool true -> VUnit
           | VBool false -> raise AssertFail
           | _ -> failwith "Assertion must be a boolean")
  
    and eval_and env e1 e2 =
      match eval env e1 with
      | VBool false -> VBool false
      | VBool true -> eval env e2
      | _ -> failwith "and requires booleans"
  
    and eval_or env e1 e2 =
      match eval env e1 with
      | VBool true -> VBool true
      | VBool false -> eval env e2
      | _ -> failwith "or requires booleans"
  
    and eval_bop op env e1 e2 =
      let v1 = eval env e1 in
      let v2 = eval env e2 in
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
  
    in eval Env.empty expr
(* ========== Interpreter ========== *)

let interp s =
  match parse s with
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
