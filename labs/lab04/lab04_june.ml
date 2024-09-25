
(*
type 'a list =
| (::) of 'a * 'a list
| []
*)

type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec reverse_append (xs : 'a list) (ys : 'a list) : 'a list =
  match xs with
  | [] -> ys
  | x :: xs -> reverse_append xs (x :: ys)

let rec rev (xs : 'a list) : 'a list =
  reverse_append xs []

(* not tail recursive *)
let rec take n xs =
  assert (n <= List.length xs);
  match xs, n with
  | _, 0 -> []
  | x :: xs', _ -> x :: take (n-1) xs'
  | _ -> assert false

let _ = assert (take 3 [0;1;2;3;4;5] = [0;1;2])

let rec every_other to_include xs =
  match (xs, to_include) with
  | [], _ -> []
  | x :: xs' , true -> x :: every_other (not to_include) xs'
  | x :: xs' , false -> every_other (not to_include) xs'

let evens xs = every_other true xs

let odds xs = every_other false xs

let _ = assert (evens [0;1;2;3;4;5] = [0;2;4])
let _ = assert (odds [0;1;2;3;4;5] = [1;3;5])

(* now rewrite as tail recursive *)
let take n xs =
  assert (n <= List.length xs);
  let rec go n xs ys =
    match xs with
    | [] -> rev ys
    | x :: xs' -> go (n-1) xs' (x :: ys)
  in
    go n xs []

let rec every_other to_include xs =
  let rec go to_include xs ys =
    match (xs, to_include) with
    | [], _ -> rev ys
    | x :: xs' , true -> go (not to_include) xs' (x :: ys)
    | x :: xs' , false -> go (not to_include) xs' ys
  in
    go to_include xs []

let evens xs = every_other true xs

let odds xs = every_other false xs
