open OUnit2
open Assign01_04

let test s e =
  let d =
    String.concat " "
      [ "to_string"
      ; string_of_int s
      ; "is"
      ; e
      ]
  in
  let a = to_string s in
  let t _ = assert_equal e a in
  d >:: t

let tests =
  "Basic to_string Examples" >:::
    [ test 1 "[]"
    ; test 2 "[1]"
    ; test 8 "[3]"
    ; test (2 * 3 * 5 * 7 * 11) "[1; 1; 1; 1; 1]"
    ; test 1058400 "[5; 3; 2; 2]"
    ]
