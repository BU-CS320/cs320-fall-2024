open My_parser
open Utils

let parse = parse

let list_hd = function
 | [] -> failwith "列表为空"
 | h :: _ -> h

let list_tl = function
 | [] -> failwith "列表为空"
 | _ :: t -> t

let rec desugar_expr (e : sfexpr) : expr =
 match e with
 | SUnit -> Unit
 | STrue -> True
 | SFalse -> False
 | SNum n -> Num n
 | SVar x -> Var x
 | SBop (op, e1, e2) -> Bop (op, desugar_expr e1, desugar_expr e2)
 | SIf (e1, e2, e3) -> If (desugar_expr e1, desugar_expr e2, desugar_expr e3)
 | SApp (e1, e2) -> App (desugar_expr e1, desugar_expr e2)
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

let desugar (prog : prog) : expr =
 let rec nest_lets = function
   | [] -> Unit
   | { is_rec; name; args; ty; value } :: rest ->
       let fun_ty =
         List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args ty
       in
       let value' =
         if args = [] then value
         else SFun { arg = list_hd args; args = list_tl args; body = value }
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

type context = (string * ty) list

let lookup ctx x =
 try Ok (List.assoc x ctx) with Not_found -> Error (UnknownVar x)

let rec type_of_expr (ctx : context) (e : expr) : (ty, error) result =
  match e with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x -> lookup ctx x
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
  | Assert e -> (
      match type_of_expr ctx e with
      | Ok BoolTy -> Ok UnitTy
      | Ok t -> Error (AssertTyErr t)
      | Error e -> Error e)
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
  | Let { is_rec; name; ty; value; body } ->
      let ctx' = if is_rec then (name, ty) :: ctx else ctx in
      (match type_of_expr ctx' value with
       | Error e -> Error e
       | Ok vty when vty = ty -> type_of_expr ((name, ty) :: ctx) body
       | Ok vty -> Error (LetTyErr (ty, vty)))

let type_of (e : expr) : (ty, error) result =
  type_of_expr [] e

