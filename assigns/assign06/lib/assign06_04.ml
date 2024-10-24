type value = 
  | VNum of int
  | VBool of bool

type expr = 
  | Num of int
  | Add of expr * expr
  | Lt of expr * expr
  | Ite of expr * expr * expr

let rec eval (e: expr) : value =
  match e with
  | Num n -> VNum n
  | Add(e1, e2) ->
    (match (eval e1, eval e2) with
     | (VNum v1, VNum v2) -> VNum (v1 + v2)
     | _ -> failwith "Type error")
  | Lt(e1, e2) ->
    (match (eval e1, eval e2) with
     | (VNum v1, VNum v2) -> VBool (v1 < v2)
     | _ -> failwith "Type error")
  | Ite(e1, e2, e3) ->
    (match eval e1 with
     | VBool true -> eval e2
     | VBool false -> eval e3
     | _ -> failwith "Type error")
