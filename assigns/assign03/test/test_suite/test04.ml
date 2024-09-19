open OUnit2
open Assign03_04

let string_of_int_list l =
  let rec inner = function
    | [] -> ""
    | [x] -> string_of_int x
    | x :: l -> string_of_int x ^ ";" ^ inner l
  in "[" ^ inner l ^ "]"

let test l e =
  let d =
    String.concat " "
      [ "testing group"
      ; string_of_int_list l
      ]
  in
  let a = group l in
  let t _ = assert_equal e a in
  d >:: t

let tests =
  "basic group examples" >:::
    [ test [1;2;3;0;-1;-2;-3;0;1;2;3] (Some [[1;2;3];[-1;-2;-3];[1;2;3]])
    ; test [1;1;0;-1;0;1] (Some [[1;1];[-1];[1]])
    ; test [1;2;3] (Some [[1;2;3]])
    ; test [1;2;3;0;-1;2;3;0] None
    ; test [0;1;2;3] None
    ; test [1;2;3;0;1;2;3] None
    ; test [1;2;3;0;0;-1;-2;-3] None
    ; test [-12;-134;-8;-6;0;1;555;435;27;0;-234;-888;-435;-1;-1;-1;0;1]
      (Some [[-12;-134;-8;-6];[1;555;435;27];[-234;-888;-435;-1;-1;-1];[1]])
    ]
