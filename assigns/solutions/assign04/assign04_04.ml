
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

type ty' =
  | Int
  | Bool

type context = (string * ty') list

let type_of' =
  let rec go ctxt = function
    | True -> Some Bool
    | False -> Some Bool
    | Num _ -> Some Int
    | Var x -> List.assoc_opt x ctxt
    | Let (x, e1, e2) -> (
      match go ctxt e1 with
      | Some t -> go ((x, t) :: ctxt) e2
      | None -> None
    )
    | Add (e1, e2) -> (
      match go ctxt e1, go ctxt e2 with
      | Some Int, Some Int -> Some Int
      | _ -> None
    )
    | Or (e1, e2) -> (
      match go ctxt e1, go ctxt e2 with
      | Some Bool, Some Bool -> Some Bool
      | _ -> None
    )
    | IfThenElse (b, e1, e2) -> (
      match go ctxt b, go ctxt e1, go ctxt e2 with
      | Some Bool, Some t1, Some t2 ->
        if t1 = t2
        then Some t1
        else None
      | _ -> None
    )
  in go
