open OUnit2
open Assign00_02

let test_example n e =
  let d =
    String.concat " "
      [ "is_prime"
      ; string_of_int n
      ; "is"
      ; string_of_bool e
      ]
  in
  let a = is_prime n in
  let t _ = assert_bool ("checking is_prime " ^ string_of_int n) (if e then a else not a) in
  d >:: t

let test_examples = "Basic is_prime examples" >:::
  [ test_example 0 false
  ; test_example 1 false
  ; test_example 2 true
  ; test_example 37 true
  ; test_example 57 false
  ; test_example 97 true
  ]

