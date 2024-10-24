type piece = 
  | X
  | O

type pos = 
  | Piece of piece
  | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
  | Top
  | Middle
  | Bottom

type col_index = 
  | Left
  | Middle
  | Right

type pos_index = row_index * col_index


let get_pos (b : board) ((r, c) : pos_index) : pos =
  let (row1, row2, row3) = b in
  let row = match r with
    | Top -> row1
    | Middle -> row2
    | Bottom -> row3
  in
  match row, c with
  | (p1, _, _), Left -> p1
  | (_, p2, _), Middle -> p2
  | (_, _, p3), Right -> p3



  let check_line (p1 : pos) (p2 : pos) (p3 : pos) : bool =
    match (p1, p2, p3) with
    | (Piece X, Piece X, Piece X) -> true
    | (Piece O, Piece O, Piece O) -> true
    | _ -> false
  
  let winner (b : board) : bool =
    let (r1, r2, r3) = b in
    let (r1c1, r1c2, r1c3) = r1 in
    let (r2c1, r2c2, r2c3) = r2 in
    let (r3c1, r3c2, r3c3) = r3 in
    (* Check rows *)
    check_line r1c1 r1c2 r1c3 ||
    check_line r2c1 r2c2 r2c3 ||
    check_line r3c1 r3c2 r3c3 ||
    (* Check columns *)
    check_line r1c1 r2c1 r3c1 ||
    check_line r1c2 r2c2 r3c2 ||
    check_line r1c3 r2c3 r3c3 ||
    (* Check diagonals *)
    check_line r1c1 r2c2 r3c3 ||
    check_line r1c3 r2c2 r3c1
  
