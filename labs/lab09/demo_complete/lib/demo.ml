open Utils
include My_parser

let rec step = function
  | Num _ -> None
  | True -> None
  | False -> None
  | Add (Num m, Num n) -> Some (Num (m + n))
  | Eq (Num m, Num n) -> Some (if m = n then True else False)
  | Eq (True, True) -> Some True
  | Eq (True, False) -> Some False
  | Eq (False, True) -> Some False
  | Eq (False, False) -> Some True
  | Add (e1, e2) -> (
    match step e1 with
    | Some e1 -> Some (Add (e1, e2))
    | None -> (
      match step e2 with
      | Some e2 -> Some (Add (e1, e2))
      | None -> None
    )
  )
  | Eq (e1, e2) -> (
    match step e1 with
    | Some e1 -> Some (Eq (e1, e2))
    | None -> (
      match step e2 with
      | Some e2 -> Some (Eq (e1, e2))
      | None -> None
    )
  )

let rec multistep e =
  match step e with
  | Some e -> multistep e
  | None -> e

let eval e =
  match multistep e with
  | Num n -> Some (VNum n)
  | True -> Some (VBool true)
  | False -> Some (VBool false)
  | _ -> None (* stuck *)

let interp s =
  match parse s with
  | None -> None
  | Some e -> eval e
