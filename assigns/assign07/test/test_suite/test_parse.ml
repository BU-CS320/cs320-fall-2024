open OUnit2
open Utils

let test s e =
  let d = "testing parse \"" ^ s ^ "\"" in
  let t _ =
    let a = My_parser.parse s in
    assert_equal e a
  in d >: test_case ~length:(Custom_length 2.) t

let basic_examples = "basic parse examples" >:::
  [ test "2 + 3" (Some (Bop (Add, Num 2, Num 3)))
  ; test "" None
  ; test "3 + 3 * 3 + 3" (Some (Bop (Add, Bop (Add, Num 3, Bop (Mul, Num 3, Num 3)), Num 3)))
  ; test "(fun x -> x + 1) 2 3 4" (Some (App (App (App (Fun ("x", Bop (Add, Var "x", Num 1)), Num 2), Num 3), Num 4)))
  ; test "let x = 2 in x" (Some (Let ("x", Num 2, Var "x")))
  ; test "let x = x in in" None
  ; test "let fun = 2 in x" None
  ; test "let f = (fun x -> x) in f ()" (Some (Let ("f", Fun ("x", Var "x"), App (Var "f", Unit))))
  ; test "2 mod (if true then 2 else 3)" (Some (Bop (Mod, Num 2, If (True, Num 2, Num 3))))
  ; test "(((( 2 <= 3 <= 4 ))))" (Some (Bop (Lte, Bop (Lte, Num 2, Num 3), Num 4)))
  ]
