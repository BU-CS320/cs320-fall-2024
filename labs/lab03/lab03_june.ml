
module HumanNames = struct

type pitch =
| A | B | C | D | E | F | G

type accidental =
| Natural | Flat | Sharp

(* note: this system isn't perfect because it does allow us
  to encode notes that don't exist like (B, Sharp) *)
(* response: just don't think about that idk *)

(* notes:
  1. constructors are always uppercase
  2. constructors must get their arguments all at once
  3. constructors are not function in ocaml
*)

type song =
| Note of pitch * accidental * song
| End

let string_of_pitch p = match p with
| A -> "A"
| B -> "B"
| C -> "C"
| D -> "D"
| E -> "E"
| F -> "F"
| G -> "G"

let string_of_accidental a = match a with
| Natural -> ""
| Sharp -> "#"
| Flat -> "b"

let rec string_of_song song = match song with
| Note (pitch, acc, song') ->
    string_of_pitch pitch ^ string_of_accidental acc ^ string_of_song song'
| End -> ""
(* END OF "HUMAN READABLE" *)
end

type nat =
| PlusOne of nat
| Zero
type nat =
| Suc of nat
| Zero

type intlist =
| Cons of int * intlist
| Nil

let rec nat_to_int n = match n with
| Suc n -> 1 + nat_to_int n
| Zero -> 0

let rec nat_to_string n = match n with
| Suc n -> "1 + " ^ nat_to_string n
| Zero -> "0"

let rec add n m = match n with
| Suc n' -> Suc (add n' m)
| Zero -> m

let rec append xs ys = match xs with
| Cons (x, xs') -> Cons (x, (append xs' ys))
| Nil -> ys

let rec string_of_intlist (xs : intlist) : string =
  match xs with
  | Nil -> "Nil"
  | Cons (x, xs) ->
    "Cons(" ^ string_of_int x ^ ", " ^ string_of_intlist xs ^ ")"


(*
Actual type of "list" in ocaml:

type 'a list =
| (::) of 'a * 'a list
| []
*)

let rec reverse_append (xs : int list) (ys : int list) : int list =
  match xs with
  | [] -> ys
  | x :: xs -> reverse_append xs (x :: ys)

let rec rev (xs : int list) : int list =
  reverse_append xs []



(* NOW DO SOME DERIVATIONS

{z : int} |- let x = 2 + z in  x + z : int

{} |- let f x = x + 1 in f 2 : int

*)





(* NOT DOING *)

let first_n xs n =
  assert (List.length xs >= n);
  let rec go xs n = match xs with
  | x :: xs' ->
    if n > 0 then
      x :: go xs' (n - 1)
    else
      []
  | _ -> assert false
  in go xs n

let rec string_of_intlist xs = match xs with
| x :: xs' -> string_of_int x ^ string_of_intlist xs'
| [] -> ""

(* let triangle xs =
  let rec loop xs n =
    print_endline (string_of_intlist (xs ))
  in
  loop xs (List.length xs) *)