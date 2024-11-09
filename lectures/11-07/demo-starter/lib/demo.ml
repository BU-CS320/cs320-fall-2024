open Utils
include My_parser

let eval =
  let rec go = function
    | True -> Some (VBool true)
    | False -> Some (VBool false)
    | Num n -> Some (VNum n)
    | Fun (x, e) -> Some (VFun (x, e))
    | Add (e1, e2) -> go_op (+) e1 e2
    | Sub (e1, e2) -> go_op (-) e1 e2
    | Mul (e1, e2) -> go_op ( * ) e1 e2
    | Eq (e1, e2) -> (
      match go e1 with
      | Some (VNum m) -> (
        match go e2 with
        | Some (VNum n) -> Some (VBool (m = n))
        | _ -> None
      )
      | _ -> None
    )
    | If (e1, e2, e3) -> (
      match go e1 with
      | Some (VBool true) -> go e2
      | Some (VBool false) -> go e3
      | _ -> None
    )
  | _ -> None
  and go_op op e1 e2 =
    match go e1 with
    | Some (VNum m) -> (
      match go e2 with
      | Some (VNum n) -> Some (VNum (op m n))
      | _ -> None
    )
    | _ -> None
  in go

let interp str =
  match parse str with
  | Some expr -> eval expr
  | None -> None
