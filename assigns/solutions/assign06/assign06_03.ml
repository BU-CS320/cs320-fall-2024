open Utils

let int_int_int t1 t2 =
  match t1, t2 with
  | Some TInt, Some TInt -> Some TInt
  | _ -> None

let int_int_bool t1 t2 =
  match t1, t2 with
  | Some TInt, Some TInt -> Some TBool
  | _ -> None

let type_of =
  let rec go = function
    | Num _ -> Some TInt
    | Add (e1, e2) -> int_int_int (go e1) (go e2)
    | Lt (e1, e2) -> int_int_bool (go e1) (go e2)
    | Ite (e1, e2, e3) -> (
      match go e1, go e2, go e3 with
      | Some TBool, Some t1, Some t2 ->
        if t1 = t2 then
        Some t1
        else None
      | _ -> None
    )
  in go
