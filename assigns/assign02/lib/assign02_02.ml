type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let rec create_rows (lst : float list) (cols : int) : float list list =
  match lst with
  | [] -> []
  | _ -> (List.take cols lst) :: (create_rows (List.drop cols lst) cols)

let mk_matrix (entries_list : float list) ((r, c) : int * int) : matrix =
  let matrix_entries = create_rows entries_list c in
  { entries = matrix_entries; rows = r; cols = c }
