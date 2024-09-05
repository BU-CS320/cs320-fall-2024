open OUnit2
open Assign01_03

let test s i e =
  let d =
    String.concat " "
      [ "nth"
      ; string_of_int s
      ; string_of_int i
      ; "is"
      ; string_of_int e
      ]
  in
  let a = nth s i in
  let t _ = assert_equal e a in
  d >:: t

let tests =
  "Basic nth tests" >:::
    [ test 4 0 2
    ; test 12 0 2
    ; test 12 1 1
    ; test (12 * 25) 2 2
    ; test (12 * 25) 1 1
    ; test 1058400 0 5
    ; test 1058400 1 3
    ; test 1058400 2 2
    ; test 1058400 3 2
    ]
