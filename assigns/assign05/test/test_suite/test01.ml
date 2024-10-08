open OUnit2
open Assign05_01

let test_list t t_name e =
  let d = "testing: fold_left cons [] " ^ t_name in
  let t _ =
    let a = fold_left (fun xs x -> x :: xs) [] t in
    assert_equal e a
  in d >: (test_case ~length:(Custom_length 1.) t)

let t1 =
  TestList
    [ TestList [TestCase 0]
    ; TestCase 1
    ; TestCase 2
    ; TestList [TestCase 3; TestCase 4]
    ]

let t2 =
  TestList
    [ TestList
      [ TestList [TestCase 0]
      ; TestCase 1
      ]
    ; TestCase 2
    ]

type test_result =
  | Success
  | Failure
  | Error
  | Timeout

let t3 =
  TestList
    [ TestCase (fun _ -> Success)
    ; TestCase (fun _ -> Success)
    ; TestList
      [TestCase (fun _ -> Failure)
      ; TestCase (fun _ -> Error)
      ]
    ]

let t4 = TestCase (fun _ -> Success)
let t5 = TestList []

type results =
  { num_succs : int
  ; num_tests : int
  }

let mk s t =
  { num_succs = s
  ; num_tests = t
  }

let all_results =
  let base = mk 0 0
  in
  let op {num_succs; num_tests} r =
    mk
      (num_succs + if r () = Success then 1 else 0)
      (num_tests + 1)
  in fold_left op base

let test_all_results t t_name e =
  let d = "testing: all_results " ^ t_name in
  let t _ =
    let a = all_results t in
    assert_equal e a
  in
  d >: test_case ~length:(Custom_length 1.) t

let basic_examples =
  "basic fold_left examples" >:::
    [ test_list t1 "t1" [4;3;2;1;0]
    ; test_list t2 "t2" [2;1;0]
    ; test_all_results t3 "t3" (mk 2 4)
    ; test_all_results t4 "t4" (mk 1 1)
    ; test_all_results t5 "t5" (mk 0 0)
    ]
