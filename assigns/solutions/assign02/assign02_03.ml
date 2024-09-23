type dir =
  | North
  | South
  | East
  | West

type path = dir list

let dist p =
  let step (x, y) = function
    | North -> (x, y + 1)
    | South -> (x, y - 1)
    | East -> (x + 1, y)
    | West -> (x - 1, y)
  in
  let rec go loc p =
    match p with
    | [] -> loc
    | d :: p -> go (step loc d) p
  in
  let dist (x, y) =
    let x = float_of_int x in
    let y = float_of_int y in
    sqrt (x *. x +. y *. y)
  in dist (go (0, 0) p)
