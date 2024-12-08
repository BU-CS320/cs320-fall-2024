(* assign04_03.ml *)
open Assign04_02

type value = 
  | VNum of int
  | VBool of bool

let rec eval = function
  | True -> VBool true
  | False -> VBool false
  | Num n -> VNum n
  | Or (e1, e2) ->
    (match eval e1, eval e2 with
    | VBool b1, VBool b2 -> VBool (b1 || b2)
    | _ -> failwith "Type error")
  | Add (e1, e2) ->
    (match eval e1, eval e2 with
    | VNum n1, VNum n2 -> VNum (n1 + n2)
    | _ -> failwith "Type error")
  | IfThenElse (e1, e2, e3) ->
    (match eval e1 with
    | VBool true -> eval e2
    | VBool false -> eval e3
    | _ -> failwith "Type error")
