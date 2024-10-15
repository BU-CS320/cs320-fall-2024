
type ident = string

type ty =
  | Unit
  | Arr of ty * ty

type expr =
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

let rec type_of ctxt e =
  match e with
  | Var x -> List.assoc_opt x ctxt
  | Fun (x, tx, e) -> (
    match type_of ((x, tx) :: ctxt) e with
    | Some t -> Some (Arr (tx, t))
    | None -> None
  )
  | App (e1, e2) -> (
    match type_of ctxt e1, type_of ctxt e2 with
    | Some Arr(t1, t2), Some t3 ->
      if t1 = t3
      then Some t2
      else None
    | _ -> None
  )
