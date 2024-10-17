type dir = 
  | North
  | South
  | East
  | West

type path = dir list


let dist (p : path) : float =
  let rec walk (p : path) (x : int) (y : int) : int * int =
    match p with
    | [] -> (x, y)
    | North :: rest -> walk rest x (y + 1)
    | South :: rest -> walk rest x (y - 1)
    | East :: rest -> walk rest (x + 1) y
    | West :: rest -> walk rest (x - 1) y
  in
  let (final_x, final_y) = walk p 0 0 in
  sqrt (float_of_int (final_x * final_x + final_y * final_y))
