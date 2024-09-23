type matrix =
  { entries : float list list
  ; rows : int
  ; cols : int
  }

let mk_matrix (l : float list) (shape : int * int) : matrix =
  let rows = fst shape in
  let cols = snd shape in
  let rec mk_entries l =
    match l with
    | [] -> []
    | l -> List.take cols l :: mk_entries (List.drop cols l)
  in
  if List.length l = rows * cols then
    let entries = mk_entries l in
    {entries;rows;cols}
  else
    { entries = []
    ; rows = -1
    ; cols = -1
    }
