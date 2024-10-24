type ty = 
  | TInt
  | TBool

type expr = 
  | Num of int
  | Add of expr * expr
  | Lt of expr * expr
  | Ite of expr * expr * expr

let rec type_of (e: expr) : ty option =
  match e with
  | Num _ -> Some TInt
  | Add(e1, e2) ->
    (match (type_of e1, type_of e2) with
     | (Some TInt, Some TInt) -> Some TInt
     | _ -> None)
  | Lt(e1, e2) ->
    (match (type_of e1, type_of e2) with
     | (Some TInt, Some TInt) -> Some TBool
     | _ -> None)
  | Ite(e1, e2, e3) ->
    (match (type_of e1, type_of e2, type_of e3) with
     | (Some TBool, Some t2, Some t3) when t2 = t3 -> Some t2
     | _ -> None)
