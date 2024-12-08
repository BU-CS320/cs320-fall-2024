open OUnit2
open Utils

let test_parse s e =
  let description = "testing parse \"" ^ s ^ "\"" in
  let t _ = assert_equal (Some e) (My_parser.parse s) in
  description >:: t

let suite =
  "Test Suite" >::: [
    test_parse "2 + 3" (Bop (Add, Num 2, Num 3));
    test_parse "let x = 2 in x" (Let ("x", Num 2, Var "x"));
    (* Add more test cases as needed *)
  ]

let () =
  run_test_tt_main suite
