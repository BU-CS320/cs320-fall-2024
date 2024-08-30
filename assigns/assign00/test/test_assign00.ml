open OUnit2

let tests = "Testing Assign00" >:::
  [ Test01.test_examples
  ; Test02.test_examples
  ]

let _ = run_test_tt_main tests
