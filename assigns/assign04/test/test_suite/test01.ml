open OUnit2
open Assign04_01

let test pred start fs fs_name e =
  let d =
    String.concat " "
      [ "testing: last_function_standing <pred>"
      ; string_of_int start
      ; fs_name
      ]
  in
  let a () = last_function_standing fs start pred in
  let cmp e_o a_o =
    match e_o, a_o with
    | Some e, Some a -> e == a
    | None, None -> true
    | _ -> false
  in
  let t _ = assert_equal e (a ()) ~cmp in
  d >:: t

let f1 n = n - 1
let f2 n = n - 2
let f3 n =
  if n = 20
  then 9
  else if n >= 10
  then n + 1
  else n - 1

let g1 n = n * n
let g2 n = n * n * n
let g3 n = 2 * n + 1000

let basic_examples =
  "basic last_function_standing examples" >:::
    [ test ((>) 0) 10 [f1;f2;f3] "[f1;f2;f3]" (Some f3)
    ; test ((>) 0) 10 [f1;f2] "[f1;f2]" (Some f1)
    ; test ((>) 0) 10 [] "[]" None
    ; test ((>) 0) 10 [f1;f1] "[f1;f1]" None
    ; test ((>) 0) 10 [f2;f3] "[f2;f3]" (Some f3)
    ; test ((<) 1000000) 2 [g1;g2;g3] "[g1;g2;g3]" (Some g3)
    ; test ((<) 1000000) 2 [g1;g2] "[g1;g2]" (Some g1)
    ; test ((<) 1000000) 2 [g1;g3] "[g1;g3]" (Some g3)
    ; test ((<) 5000) 2 [g1;g3] "[g1;g3]" (Some g1)
    ; test ((<) 3000) 10 [g1;g3] "[g1;g3]" None
    ; test ((<) 123123) 123123 [g1] "[g1]" (Some g1)
    ]

