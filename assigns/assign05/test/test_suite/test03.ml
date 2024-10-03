open OUnit2
open Assign05_03

let test (ctxt, exp, e) tname =
  let d = "testing: " ^ tname in
  let t _ = assert_equal e (type_of ctxt exp) in
  d >: test_case ~length:(Custom_length 1.) t

let t0 =
  ( []
  , Fun ("x", Unit, Var "x")
  , Some (Arr (Unit, Unit))
  )

let t1 =
  ( []
  , Fun
    ( "f"
    , Arr (Unit, Unit)
    , Var "f"
    )
  , Some (Arr (Arr (Unit, Unit), Arr (Unit, Unit)))
  )

let t2 =
  ( [ "y", Arr (Unit, Unit) ]
  , Fun
    ( "x"
    , Unit
    , App (Var "y", Var "x")
    )
  , Some (Arr (Unit, Unit))
  )

let t3 =
  ( []
  , App
    ( Fun ("x", Unit, Var "x")
    , Fun ("x", Unit, Var "x")
    )
  , None
  )

let t4 =
  ( []
  , Fun
    ( "f"
    , Arr (Unit, Unit)
    , Fun
      ( "x"
      , Unit
      , App (Var "f", Var "x")
      )
    )
  , Some (Arr (Arr (Unit, Unit), Arr (Unit, Unit)))
  )

let basic_examples = "basic type_of examples" >:::
  [ test t0 "t0"
  ; test t1 "t1"
  ; test t2 "t2"
  ; test t3 "t3"
  ; test t4 "t4"
  ]
