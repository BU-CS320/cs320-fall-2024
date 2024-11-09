open Utils

let eval =
  let rec go = function
    | Num n -> VNum n
    | Add (e1, e2) -> bop (+) e1 e2
    | Lt (e1, e2) -> bop' (<) e1 e2
    | Ite (e1, e2, e3) -> (
      match go e1 with
      | VBool true -> go e2
      | VBool false -> go e3
      | _ -> failwith "type check failed"
    )
  and bop f e1 e2 =
    match go e1, go e2 with
    | VNum m, VNum n -> VNum (f m n)
    | _ -> failwith "type check failed"
  and bop' f e1 e2 =
    match go e1, go e2 with
    | VNum m, VNum n -> VBool (f m n)
    | _ -> failwith "type check failed"
  in go
