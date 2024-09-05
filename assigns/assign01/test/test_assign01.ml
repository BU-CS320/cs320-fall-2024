open OUnit2

let tests =
  "Testing Assign01" >:::
    [ Test01.tests
    ; Test02.tests
    ; Test03.tests
    ; Test04.tests
    ]

let _ = run_test_tt_main tests
