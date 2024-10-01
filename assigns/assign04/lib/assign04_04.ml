(* assign04_04.ml *)

type ident = string

type expr' =
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' = Int | Bool
type context = (ident * ty') list

(* Helper function to look up variable types in the context *)
let rec lookup ctx x =
  match ctx with
  | [] -> None
  | (y, t) :: ys -> if x = y then Some t else lookup ys x

(* Type checker for the extended language with variables and let-bindings *)
let rec type_of' ctx = function
  | True | False -> Some Bool
  | Num _ -> Some Int
  | Var x -> lookup ctx x
  | Add (e1, e2) ->
    (match type_of' ctx e1, type_of' ctx e2 with
    | Some Int, Some Int -> Some Int
    | _ -> None)
  | Or (e1, e2) ->
    (match type_of' ctx e1, type_of' ctx e2 with
    | Some Bool, Some Bool -> Some Bool
    | _ -> None)
  | IfThenElse (e1, e2, e3) ->
    (match type_of' ctx e1, type_of' ctx e2, type_of' ctx e3 with
    | Some Bool, Some t2, Some t3 when t2 = t3 -> Some t2
    | _ -> None)
  | Let (x, e1, e2) ->
    (match type_of' ctx e1 with
    | Some t1 -> type_of' ((x, t1) :: ctx) e2
    | _ -> None)

