
let rec add_int_list n xs =
  match xs with
  | [] -> []
  | x :: xs -> n + x :: add_int_list n xs

let rec mult_int_list n xs =
  match xs with
  | [] -> []
  | x :: xs -> n * x :: mult_int_list n xs

let rec exclaim xs =
  match xs with
  | [] -> []
  | x :: xs -> (x ^ "!") :: exclaim xs

let rec sum xs =
  let rec go acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> go (acc + x) xs
  in
    go 0 xs

let _ = assert (sum [1;2;3;4;5;6;7;8;9] = 45)
let _ = print_int (sum [1;2;3;4;5;6;7;8;9])
let _ = print_endline ""

let join_strings separator xs =
  let rec go acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> go (acc ^ x ^ separator) xs
  in
    go "" xs

let _ = print_endline (join_strings " " ["hi"; "everyone,"; "how"; "are"; "you?"])
let _ = print_endline (join_strings "___" ["hi"; "everyone,"; "how"; "are"; "you?"])

(* now lets make an abstraction *)

let rec map f xs =
  match xs with
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec foldl f acc xs =
  match xs with
  | [] -> acc
  | x :: xs -> foldl f (f acc x) xs

type 'a tree =
| Leaf of 'a
| Node of 'a tree * 'a * 'a tree

let rec map_tree f t =
  match t with
  | Leaf x -> Leaf (f x)
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)