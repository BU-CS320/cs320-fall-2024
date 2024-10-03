open OUnit2
open Assign05_02

let test t t_name e =
  let d = "testing: sum_tr " ^ t_name in
  let t _ = assert_equal e (sum_tr t) in
  d >: test_case ~length:(Custom_length 1.) t

let t1 =
  Node
  ( 13
  , Node
    ( 12
    , Node
      (-3
      , Leaf
      , Leaf
      )
    , Leaf
    )
  , Node
    ( 99
    , Leaf
    , Leaf
    )
  )

let t2 =
  let rec go i =
    if i = 0
    then Leaf
    else Node (1, go (i - 1), go (i - 1))
  in go 10

let test_tr =
  let tree =
    let rec go acc i =
      if i = 0
      then acc
      else go (Node (0, acc, Leaf)) (i - 1)
    in go Leaf 10000000
  in
  let t _ = assert_equal 0 (sum_tr tree) in
  "tail recusion test" >: test_case ~length:(Custom_length 2.0) t

let basic_examples = "basic sum_tr examples" >:::
  [ test t1 "t1" (13 + 12 - 3 + 99)
  ; test t2 "t2" 1023
  ; test_tr
  ]
