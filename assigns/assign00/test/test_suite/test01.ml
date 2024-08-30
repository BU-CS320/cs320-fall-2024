open OUnit2
open Assign00_01

let test_example n e =
  let d =
    String.concat " "
      [ "sqrt"
      ; string_of_int n
      ; "is"
      ; string_of_int e
      ]
  in
  let a = sqrt n in
  let t _ = assert_equal e a in
  d >:: t

let test_examples = "Basic sqrt Examples" >:::
  [ test_example 4 2
  ; test_example 9 3
  ; test_example 100 10
  ; test_example 2 2
  ; test_example 10 4
  ; test_example 99 10
  ]
