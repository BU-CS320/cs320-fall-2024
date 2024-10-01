open OUnit2
open Assign04_02

let string_of_expr =
  let rec go = function
    | True -> "true"
    | False -> "false"
    | Num n ->
      let s = string_of_int n in
      if n < 0
      then "(" ^ s ^ ")"
      else s
    | Or (e1, e2) ->
      String.concat ""
      [ "("
      ; go e1
      ; " || "
      ; go e2
      ; ")"
      ]
    | Add (e1, e2) ->
      String.concat ""
      [ "("
      ; go e1
      ; " + "
      ; go e2
      ; ")"
      ]
    | IfThenElse (b, e1, e2) ->
      String.concat ""
      [ "("
      ; "if "
      ; go b
      ; " then "
      ; go e1
      ; " else "
      ; go e2
      ; ")"
      ]
  in go

let test exp e =
  let d =
    String.concat " "
    [ "testing: type_of"
    ; string_of_expr exp
    ]
  in
  let a () = type_of exp in
  let printer = function
   | Some Int -> "int"
   | Some Bool -> "bool"
   | None -> "does not type check"
  in
  let t _ = assert_equal e (a ()) ~printer in
  d >:: t

let basic_examples =
  "basic type_of examples" >:::
  [ test (Num 2) (Some Int)
  ; test True (Some Bool)
  ; test (Add (Num 2, Num 3)) (Some Int)
  ; test (Or (True, Num 3)) None
  ; test (IfThenElse (True, Num 2, Num 3)) (Some Int)
  ; test (IfThenElse (True, False, True)) (Some Bool)
  ; test (IfThenElse (Or (True, True), Num 2, Add (Num 2, Num 3))) (Some Int)
  ; test (IfThenElse (Num 2, Num 3, Num 3)) None
  ; test (IfThenElse (Or (True, Num 3), True, True)) None
  ; test (Add (Num 3, IfThenElse (True, Num 2, Num 3))) (Some Int)
  ]

