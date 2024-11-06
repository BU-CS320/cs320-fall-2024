open Utils

let parse =
  let rec go acc l =
    match acc, l with
    | [out], [] -> Some out
    | _, TNum n :: l -> go (Num n :: acc) l
    | e2 :: e1 :: acc, TAdd :: l -> go (Add (e1, e2) :: acc) l
    | e2 :: e1 :: acc, TLt :: l -> go (Lt (e1, e2) :: acc) l
    | e3 :: e2 :: e1 :: acc, TIte :: l -> go (Ite (e1, e2, e3) :: acc) l
    | _ -> None
  in go []
