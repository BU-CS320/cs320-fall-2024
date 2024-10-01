
(* type 'a list =
| (::) of 'a * 'a list
| [] *)

type 'a tree =
| Node of 'a tree * 'a * 'a tree
| Leaf


(* let rec reverse_append (xs : 'a list) (ys : 'a list) : 'a list = *)
let rec reverse_append xs ys =
  match xs with
  | [] -> ys
  | x :: xs' -> reverse_append xs' (x :: ys)

(* reverse_append [0;1;2] [3;4;5]
reverse_append [1;2] [0;3;4;5]
reverse_append [2] [1;0;3;4;5]
reverse_append [] [2;1;0;3;4;5]
[2;1;0;3;4;5] *)

let _ = assert (reverse_append [0;1;2] [3;4;5] = [2;1;0;3;4;5])

let rec rev (xs : 'a list) : 'a list =
  reverse_append xs []

let _ = assert (rev [0;1;2] = [2;1;0])
let _ = print_int (List.nth (rev [0;1;2]) 0)

exception YouAskedForTooManyItemsDummy

(* not tail recursive *)
let rec take (n : int) (xs : 'a list) : 'a list =
  if not (n <= List.length xs) then
    raise YouAskedForTooManyItemsDummy
  else
  match n, xs with
  | 0, _ -> []
  | _, x :: xs' -> x :: take (n-1) xs'
  | _ -> assert false

let take n xs =
  assert (n <= List.length xs);
  let rec go n xs ys =
    match n, xs with
    | 0, _ -> rev ys
    | _, x :: xs' -> go (n-1) xs' (x :: ys)
    | _ -> assert false
  in
    go n xs []

(* let _ = try (take 10000 [0;1;2;3;4;5] = [0;1;2])
  with YouAskedForTooManyItemsDummy -> print_endline "woops!"; false *)
(* let _ = assert (take 3 [0;1;2;3;4;5] = [2;1;0]) *)
let _ = assert (take 3 [0;1;2;3;4;5] = [0;1;2])

(* let rec every_other to_include xs =
  assert false

let evens xs = every_other true xs

let odds xs = every_other false xs

let _ = assert (evens [0;1;2;3;4;5] = [0;2;4])
let _ = assert (odds [0;1;2;3;4;5] = [1;3;5])

(* now rewrite as tail recursive *)
let take n xs =
  assert false

let rec every_other to_include xs =
  assert false

let evens xs = assert false

let odds xs = assert false *)
