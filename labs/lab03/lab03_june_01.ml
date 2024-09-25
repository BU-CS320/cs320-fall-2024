

type pitch = A | B | C | D | E | F | G

type accidental = Flat | Natural | Sharp

type song =
| Note of pitch * accidental * song
| End

let string_of_pitch pitch = match pitch with
| A -> "A"
| B -> "B"
| C -> "C"
| D -> "D"
| E -> "E"
| F -> "F"
| G -> "G"

let string_of_accidental acc = match acc with
| Flat -> "b"
| Natural -> ""
| Sharp -> "#"

let _ = print_endline (string_of_pitch A ^ string_of_accidental Sharp)

let rec string_of_song song = match song with
| Note (p, a, s) ->
    string_of_pitch p ^ string_of_accidental a ^ " " ^ string_of_song s
| End -> ""

let _ = print_endline (string_of_song (
  Note (A, Sharp, Note (B, Natural, Note (D, Flat, End)))
))


type song = (pitch * accidental) list

type list =
| Cons of int * list
| Nil

type 'a list =
| (::) of 'a * 'a list
| []

type nat =
| PlusOne of nat
| Zero

type nat =
| Suc of nat
| Zero


let rec append xs ys = match xs with
| x :: xs' -> x :: append xs' ys
| [] -> ys

let rec reverse_append xs ys = match xs with
| x :: xs'-> reverse_append xs' (x :: ys)
| [] -> ys

let reverse xs = reverse_append xs []


let _ = assert (reverse_append [1;2;3] [4;5;6] = [3;2;1;4;5;6])