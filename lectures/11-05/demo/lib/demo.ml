open Utils
include My_parser

(*

--------------------------
fun x -> e \|/ fun x -> e

e1 \|/ fun x -> e      e2 \|/ v2      [v2 / x] e \|/ v
------------------------------------------------------
              e1 e2 \|/ v

[v / x] y = if x = y then v else x

[v / x] (e1 e2) = ([v / x] e1) ([v / x] e2)

[v / x] (fun y -> e) =
  if x = y then
    fun y -> x
  else
    fun z -> [v / x] ([z / y] e)

*)

let _ = ignore (Var "x")
