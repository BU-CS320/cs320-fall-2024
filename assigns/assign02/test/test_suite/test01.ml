open OUnit2
open Assign02_01

let string_of_pos = function
  | Piece X -> "X"
  | Piece O -> "O"
  | Blank -> "_"

let string_of_row (p1, p2, p3) =
  String.concat " "
    [ string_of_pos p1
    ; string_of_pos p2
    ; string_of_pos p3
    ]

let string_of_board (r1, r2, r3) =
  String.concat "\n"
    [ string_of_row r1
    ; string_of_row r2
    ; string_of_row r3
    ]

let string_of_row_index = function
  | Top -> "Top"
  | Middle -> "Middle"
  | Bottom -> "Bottom"

let string_of_col_index = function
  | Left -> "Left"
  | Middle -> "Middle"
  | Right-> "Right"

let string_of_pos_index (ri, ci) =
  "("
  ^ string_of_row_index ri
  ^ ", "
  ^ string_of_col_index ci
  ^ ")"

let get_pos_test b pi e =
  let d =
    String.concat "\n"
      [ ""
      ; "testing get_pos at"
      ; string_of_pos_index pi
      ; "on board"
      ; string_of_board b
      ]
  in
  let a = get_pos b pi in
  let t _ = assert_equal e a in
  d >:: t

let b1 =
  ( (Piece X, Piece O, Piece X)
  , (Blank, Piece X, Piece O)
  , (Blank, Blank, Piece X)
  )

let b2 =
  ( (Piece O, Piece O, Piece X)
  , (Piece X, Piece X, Piece O)
  , (Blank, Blank, Piece O)
  )

let get_pos_tests =
  "basic get_pos examples" >:::
    [ get_pos_test b1 (Middle, Middle) (Piece X)
    ; get_pos_test b1 (Bottom, Left) Blank
    ; get_pos_test b2 (Top, Left) (Piece O)
    ]

let winner_test b e =
  let d =
    String.concat "\n"
      [ "testing winner on"
      ; string_of_board b
      ]
  in
  let a = winner b in
  let t _ = assert_equal e a in
  d >:: t

let b3 =
  ( (Piece X, Blank, Blank)
  , (Piece X, Blank, Blank)
  , (Piece X, Blank, Blank)
  )

let winner_tests =
  "basic winner examples" >:::
    [ winner_test b1 true
    ; winner_test b2 false
    ; winner_test b3 true
    ]

let tests =
  "basic tic-tac-toe test" >:::
    [ get_pos_tests
    ; winner_tests
    ]
