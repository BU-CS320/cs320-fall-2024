open OUnit2
open Utils
open Assign06_02

let string_of_tok = function
  | TAdd -> "+"
  | TLt -> "<"
  | TIte -> "?"
  | TNum n -> string_of_int n

let string_of_list f l =
  let rec inner = function
    | [] -> ""
    | [x] -> f x
    | x :: l -> f x ^ "; " ^ inner l
  in "[" ^ inner l ^ "]"

let str = string_of_list string_of_tok

let test tks e =
  let d =
    String.concat " "
      [ "testing parse"
      ; str tks
      ]
  in
  let t _ = assert_equal e (parse tks) in
  d >: test_case ~length:(Custom_length 2.) t

let basic_examples = "basic parse examples" >:::
  [ test [TNum 2] (Some (Num 2))
  ; test [TNum 2; TNum 3; TLt] (Some (Lt (Num 2, Num 3)))
  ; test [TNum 2; TNum 3; TAdd; TAdd] None
  ; test
    [TNum 2; TNum 3; TAdd; TNum 3; TNum 3; TAdd; TLt; TNum 0; TNum 1; TNum 1; TAdd; TIte]
    (Some (Ite (Lt (Add (Num 2, Num 3), Add (Num 3, Num 3)), Num 0, Add (Num 1, Num 1))))
  ; test
    [TNum 2; TNum 3; TAdd; TNum 3; TNum 3; TAdd; TLt; TNum 0; TNum 1; TNum 1; TAdd]
    None
  ]
