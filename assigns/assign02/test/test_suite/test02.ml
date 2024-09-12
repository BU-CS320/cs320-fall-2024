open OUnit2
open Assign02_02

let string_of_int_pair p =
  String.concat ""
    [ "("
    ; string_of_int (fst p)
    ; ", "
    ; string_of_int (snd p)
    ; ")"
    ]

let test l shape e =
  let d =
    String.concat " "
      [ "testing mk_matrix on list of length"
      ; string_of_int (List.length l)
      ; "and shape"
      ; string_of_int_pair shape
      ]
  in
  let a = mk_matrix l shape in
  let t _ = assert_equal e a in
  d >:: t

let id2 =
  { entries =
      [ [1.;0.]
      ; [0.;1.]
      ]
  ; rows = 2
  ; cols = 2
  }

let simple_matrix =
  { entries =
      [ [1.;2.;3.]
      ; [4.;5.;6.]
      ; [7.;8.;9.]
      ]
  ; rows = 3
  ; cols = 3
  }

let tests =
  "basic mk_matrix examples" >:::
    [ test [1.;0.;0.;1.] (2, 2) id2
    ; test [1.;2.;3.;4.;5.;6.;7.;8.;9.] (3, 3) simple_matrix
    ; test [] (200, 0) {entries=[];rows=200;cols=0}
    ; test [] (0, 200) {entries=[];rows=0;cols=200}
    ]
