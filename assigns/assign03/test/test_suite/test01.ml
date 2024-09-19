open OUnit2
open Assign03_01

let test_value l l_name key e =
  let d =
    String.concat " "
      [ "testing mk_unique_keys"
      ; l_name
      ; key
      ]
  in
  let a = List.assoc key (mk_unique_keys l) in
  let t _ = assert_equal e a in
  d >:: t

let test_length l l_name e =
  let d =
    String.concat " "
      [ "testing length of mk_unique_keys"
      ; l_name
      ]
  in
  let a = List.length (mk_unique_keys l) in
  let t _ = assert_equal e a in
  d >:: t

let l1 = [("the", 1); ("cat", 1); ("in", 1); ("the", 1); ("hat", 1)]
let l2 = [("a", 3); ("b", -4); ("a", -2)]

let tests =
  "basic mk_matrix examples" >:::
    [ test_value l1 "l1" "cat" 1
    ; test_value l1 "l1" "in" 1
    ; test_value l1 "l1" "hat" 1
    ; test_value l1 "l1" "the" 2
    ; test_length l1 "l1" 4
    ; test_value l2 "l2" "a" 1
    ; test_value l2 "l2" "b" (-4)
    ; test_length l2 "l2" 2
    ]
