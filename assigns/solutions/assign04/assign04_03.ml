open Assign04_02

type value =
  | VNum of int
  | VBool of bool

let eval =
  let rec go = function
    | True -> VBool true
    | False -> VBool false
    | Num n -> VNum n
    | Or (b1, b2) -> (
      match go b1, go b2 with
      | VBool b1, VBool b2 -> VBool (b1 || b2)
      | _ -> assert false
    )
    | Add (e1, e2) -> (
      match go e1, go e2 with
      | VNum n1, VNum n2 -> VNum (n1 + n2)
      | _ -> assert false
    )
    | IfThenElse (b, e1, e2) -> (
      match go b, go e1, go e2 with
      | VBool b, v1, v2 -> if b then v1 else v2
      | _ -> assert false
    )
  in go
