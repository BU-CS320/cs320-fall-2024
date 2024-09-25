open OUnit2
open Assign03_02

let test start start_name index e =
  let d =
    String.concat " "
      [ "testing gen_fib"
      ; start_name
      ; "up to index"
      ; string_of_int index
      ]
  in
  let a = List.init (index + 1) (fun index -> gen_fib start index) in
  let t _ = assert_equal e a in
  d >:: t

let test_tail_rec =
  let a () = gen_fib (List.init 1000 (fun _ -> 0)) 100000 in
  let t _ = assert_equal (a ()) 0 in
  OUnitTest.TestCase (OUnitTest.Custom_length 2., t)

let tests =
  "basic gen_fib examples" >:::
    [ test [1;1] "fib" 7 [1;1;2;3;5;8;13;21]
    ; test [1;2;3] "[1;2;3]" 7 [1;2;3;6;11;20;37;68]
    ; test [0;0;0] "zeros" 7 [0;0;0;0;0;0;0;0]
    ; test_tail_rec
    ]
