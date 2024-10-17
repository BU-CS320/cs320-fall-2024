open OUnit2
open Utils
open Assign06_01

let test s e =
  let d = "testing lex \"" ^ s ^ "\"" in
  let t _ =
    let a = lex s in
    assert_equal e a
  in d >: test_case ~length:(Custom_length 2.) t

let basic_examples = "basic lex examples" >:::
  [ test "2 + 3" (Some [TNum 2; TAdd; TNum 3])
  ; test "+ \n + 0  \n -0" (Some [TAdd; TAdd; TNum 0; TNum 0])
  ; test "   + + + + + + + + + + + + + -  " None
  ; test
      "2 3 + 3 3 + < 0 1 1 + ?"
      (Some [TNum 2; TNum 3; TAdd; TNum 3
            ; TNum 3; TAdd; TLt; TNum 0;
            TNum 1; TNum 1; TAdd; TIte])
  ; test "2 3 + 3 3 + < 0 1 1 +?" None
  ]
