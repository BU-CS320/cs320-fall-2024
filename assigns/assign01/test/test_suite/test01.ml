open OUnit2
open Assign01_01

let test n k e =
  let d =
    String.concat " "
      [ "pow"
      ; string_of_int n
      ; string_of_int k
      ; "is"
      ; string_of_int e
      ]
  in
  let a = pow n k in
  let t _ = assert_equal e a in
  d >:: t

let tests =
  "Basic pow Examples" >:::
    [ test 2 3 8
    ; test 2 4 16
    ; test (-2) 3 (-8)
    ; test 3 4 81
    ; test (-3) 4 81
    ; test 34 0 1
    ; test (-34) 0 1
    ; test 7 4 2401
    ]
