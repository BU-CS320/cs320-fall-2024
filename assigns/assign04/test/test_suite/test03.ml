open OUnit2
open Assign04_03

let test exp e =
  let d =
    String.concat " "
      [ "testing: "
      ; "eval"
      ; Test02.string_of_expr exp
      ]
  in
  let a () = eval exp in
  let printer = function
    | VBool b -> string_of_bool b
    | VNum n -> string_of_int n
  in
  let t _ = assert_equal e (a ()) ~printer in
  d >:: t

let basic_examples =
  "basic eval examples" >:::
  [ test (Num 2) (VNum 2)
  ; test True (VBool true)
  ; test (Add (Num 2, Num 3)) (VNum 5)
  ; test (Add (Num 2, IfThenElse (Or (True, False), Num (-2), Add (Num 2, Num 3)))) (VNum 0)
  ; test (IfThenElse (False, Or (True, False), Or (False, False))) (VBool false)
  ]
