let mk_unique_keys alst =
  let rec insert (k, v) acc =
    match acc with
    | [] -> [(k, v)]
    | (k', v')::t when k = k' -> (k', v' + v)::t
    | h::t -> h::(insert (k, v) t)
  in
  List.fold_left (fun acc (k, v) -> insert (k, v) acc) [] alst
