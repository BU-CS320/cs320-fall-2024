
let mk_unique_keys : (string * int) list -> (string * int) list =
  let rec insert (x, v) l =
    match l with
    | [] -> [(x, v)]
    | (y, w) :: l ->
       if x = y
       then (x, w + v) :: l
       else (y, w) :: insert (x, v) l
  in
  let rec go out l =
    match l with
    | [] -> out
    | x :: l -> go (insert x out) l
  in
  go []
