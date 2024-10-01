open OUnit2
open Assign04_04

let test d (ctxt, exp) e =
  d >:: fun _ -> assert_equal e (type_of' ctxt exp)

(* { z : int } |- let x = z + 5 in x + z : int *)
let t1 =
  ( ["z", Int]
  , Let
      ( "x"
      , Add (Var "z", Num 5)
      , Add (Var "x", Var "z")
      )
  )

(* {} |- if true then false else true : bool *)
let t2 =
  ( []
  , IfThenElse (True, False, True)
  )

(* {} |- let x = let y = let z = 5 in z + z in y + y in x + x  : int *)
let t3 =
  ( []
  , Let
    ( "x"
    , Let
      ( "y"
      , Let
        ( "z"
        , Num 5
        , Add (Var "z", Var "z")
        )
      , Add (Var "y", Var "y")
      )
    , Add (Var "x", Var "x")
    )
  )

(* { b : bool } |- if b then 3 + 4 else 3 + z : ??? *)
let t4 =
  ( [ "b", Bool ]
  , IfThenElse
    ( Var "b"
    , Add (Num 3, Num 4)
    , Add (Num 3, Var "z")
    )
  )

(* { x : int, y : bool } |- (let b = true in if b || y then x else 3) + 5 *)
let t5 =
  ( [ "x", Int; "y", Bool ]
  , Add
    ( Let
      ( "b"
      , True
      , IfThenElse
        (Or (Var "b", Var "y")
        , Var "x"
        , Num 3
        )
      )
    , Num 5
    )
  )

let basic_examples =
  "basic type_check examples" >:::
  [ test "testing t1" t1 (Some Int)
  ; test "testing t2" t2 (Some Bool)
  ; test "testing t3" t3 (Some Int)
  ; test "testing t4" t4 None
  ; test "testing t5" t5 (Some Int)
  ]

