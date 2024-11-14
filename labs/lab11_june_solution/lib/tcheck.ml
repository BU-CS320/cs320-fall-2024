
open Syntax
(* open Format *)

type ctx = (string * ty) list
(* let rec pp_ctx fmt (ctx : ctx) =
  fprintf fmt "[";
  List.iter (fun (x, v) -> fprintf fmt "(%s, %a); " x pp_ty v) ctx;
  fprintf fmt "]" *)

let ty_equal t1 t2 =
  if t1 = t2 then ()
  else failwith "type error"

let rec infer (ctx : ctx) (e : expr) : ty =
  match e with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | UnaryOp (Neg, e) ->
    check ctx e TInt;
    TInt
  | UnaryOp (Not, e) ->
    check ctx e TBool;
    TBool
  | BinaryOp ((Add | Sub | Mul | Div), e1, e2) ->
    check ctx e1 TInt;
    check ctx e2 TInt;
    TInt
  | BinaryOp ((And | Or), e1, e2) ->
    check ctx e1 TBool;
    check ctx e2 TBool;
    TBool
  | BinaryOp ((Eq | Neq | Lt | Gt | Lte | Gte), e1, e2) ->
    check ctx e1 TInt;
    check ctx e2 TInt;
    TBool
  | Var x -> (
      try List.assoc x ctx with
      | Not_found -> failwith ("undefined variable " ^ x)
    )
  | Ann (e, t) -> check ctx e t; t
  | Let (x, e1, e2) ->
    let t_e1 = infer ctx e1 in
    infer ((x, t_e1) :: ctx) e2
  | Fun _ -> failwith "unannotated function"
  | App (fn, arg) -> (
      let t_fn = infer ctx fn in
      match t_fn with
      | TArrow (t1, t2) -> check ctx arg t1; t2
      | _ -> failwith "type error in function application"
    )
  | Ifte (guard, b_then, b_else) ->
    let t_guard = infer ctx guard in
    let t_then = infer ctx b_then in
    let t_else = infer ctx b_else in
    ty_equal t_guard TBool;
    ty_equal t_then t_else;
    t_then

and check ctx e t : unit =
  match e, t with
  | Fun (f, x, body), TArrow (t1, t2) ->
    check ((f, TArrow (t1, t2)) :: (x, t1) :: ctx) body t2
  | _ -> ty_equal t (infer ctx e)
