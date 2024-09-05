open OUnit2
open Assign01_02

let test i e =
  let d =
    String.concat " "
      [ "nth_prime"
      ; string_of_int i
      ; "is"
      ; string_of_int e
      ]
  in
  let a = nth_prime i in
  let t _ = assert_equal e a in
  d >:: t

let tests =
  "Basic nth_prime tests" >:::
    [ test 0 2
    ; test 1 3
    ; test 2 5
    ; test 3 7
    ; test 998 7907
    ; test 999 7919
    ]
