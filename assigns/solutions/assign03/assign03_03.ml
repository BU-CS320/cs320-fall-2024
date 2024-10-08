
type tree =
  | Leaf of int
  | Node of tree list

let full_collapse =
  let rec full_collapse_single t =
    match t with
    | Leaf n -> [ Leaf n ]
    | Node [] -> [ Node [] ]
    | Node cs -> go cs
  and go cs =
    match cs with
    | [] -> []
    | c :: cs -> full_collapse_single c @ go cs
  in go

let rec collapse h t =
  let rec go cs =
    match cs with
    | [] -> []
    | c :: cs -> collapse (h - 1) c :: go cs
  in
  match t with
  | Leaf n -> Leaf n
  | Node cs ->
     if h <= 1
     then Node (full_collapse cs)
     else Node (go cs)
