let rec add_to_int_list n xs =
  match xs with
  | [] -> []
  | x :: xs -> x + n :: add_to_int_list n xs

let rec mult_int_list n xs =
  match xs with
  | [] -> []
  | x :: xs -> x * n :: mult_int_list n xs

let rec exclaim xs =
  match xs with
  | [] -> []
  | x :: xs -> (x ^ "!") :: exclaim xs

let rec map f xs =
  match xs with
  | [] -> []
  | x :: xs -> f x :: map f xs

let add_to_int_list n xs =
  map (fun x -> x + n) xs

let mult_int_list n xs =
  map (fun x -> x * n) xs

let exclaim xs =
  map (fun x -> x ^ "!") xs

let add_n(n, x) = n + x

let _ = add_n(1,2)

let add_n n x =
  n + x

let add_n =
  fun n ->
    fun x ->
      n + x

let add5 = add_n 5

let add10 = add_n 10

let add_5_to_int_list n xs =
  map add5 xs

let _ = print_endline (string_of_int (add5 5))
let _ = print_endline (string_of_int (add10 5))

let add_to_int_list n xs =
  map (add_n n) xs

(* FOLD *)

let sum xs =
  let rec go acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> go (acc + x) xs
  in
    go 0 xs

let join_strings separator xs =
  let rec go acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> go (acc ^ x ^ separator) xs
  in
    go "" xs

let rec fold f acc xs =
  match xs with
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs

let sum xs = fold (fun acc x -> acc + x) 0 xs

let join_strings separator xs =
  fold (fun acc x -> acc ^ x ^ separator) "" xs

let _ = assert (sum [1;2;3;4;5;6;7;8;9] = 45)
let _ = print_int (sum [1;2;3;4;5;6;7;8;9])
let _ = print_endline ""

let _ = print_endline (join_strings " " ["hi"; "everyone,"; "how"; "are"; "you?"])
let _ = print_endline (join_strings "___" ["hi"; "everyone,"; "how"; "are"; "you?"])

type 'a tree =
| Leaf of 'a
| Node of 'a tree * 'a * 'a tree

let rec map_tree f t =
  match t with
  | Leaf x -> Leaf (f x)
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)


