(* assign04_02.ml *)
type expr =
  | True
  | False
  | Num of int
  | Or of expr * expr
  | Add of expr * expr
  | IfThenElse of expr * expr * expr

type ty = Int | Bool

let rec type_of = function
  | True | False -> Some Bool
  | Num _ -> Some Int
  | Or (e1, e2) ->
    (match type_of e1, type_of e2 with
    | Some Bool, Some Bool -> Some Bool
    | _ -> None)
  | Add (e1, e2) ->
    (match type_of e1, type_of e2 with
    | Some Int, Some Int -> Some Int
    | _ -> None)
  | IfThenElse (e1, e2, e3) ->
    (match type_of e1, type_of e2, type_of e3 with
    | Some Bool, Some t2, Some t3 when t2 = t3 -> Some t2
    | _ -> None)
