type piece = X | O
type pos = Piece of piece | Blank
type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = Top | Middle | Bottom
type col_index = Left | Middle | Right

type pos_index = row_index * col_index

let get_row (t, m, b) ri =
  match ri with
  | Top -> t
  | Middle -> m
  | Bottom -> b

let get_col_pos (l, m, r) ci =
  match ci with
  | Left -> l
  | Middle -> m
  | Right -> r

let get_pos b (ri, ci) = get_col_pos (get_row b ri) ci

let get_col b ci =
  ( get_pos b (Top, ci)
  , get_pos b (Middle, ci)
  , get_pos b (Bottom, ci)
  )

let win r =
  match r with
  | (Piece X, Piece X, Piece X) -> true
  | (Piece O, Piece O, Piece O) -> true
  | _ -> false

let diag1 b =
  ( get_pos b (Top, Left)
  , get_pos b (Middle, Middle)
  , get_pos b (Bottom, Right)
  )

let diag2 b =
  ( get_pos b (Top, Right)
  , get_pos b (Middle, Middle)
  , get_pos b (Bottom, Left)
  )

let winner b =
  win (get_row b Top)
  || win (get_row b Middle)
  || win (get_row b Bottom)
  || win (get_col b Left)
  || win (get_col b Middle)
  || win (get_col b Right)
  || win (diag1 b)
  || win (diag2 b)
