
type expr =
  | True
  | False
  | Num of int
  | Or of expr * expr
  | Add of expr * expr
  | IfThenElse of expr * expr * expr

type ty =
  | Int
  | Bool

let type_of =
  let rec go = function
    | True -> Some Bool
    | False -> Some Bool
    | Num _ -> Some Int
    | Or (b1, b2) -> (
      match go b1, go b2 with
      | Some Bool, Some Bool -> Some Bool
      | _ -> None
    )
    | Add (e1, e2) -> (
      match go e1, go e2 with
      | Some Int, Some Int -> Some Int
      | _ -> None
    )
    | IfThenElse (b, e1, e2) -> (
      match go b, go e1, go e2 with
      | Some Bool, Some t1, Some t2 ->
        if t1 = t2
        then Some t1
        else None
      | _ -> None
    )
  in go
