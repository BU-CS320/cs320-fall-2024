
type pitch = A | B | C | D | E | F | G

type accidental = Natural | Sharp | Flat

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
| Natural -> ""
| Flat -> "b"
| Sharp -> "#"

let _ = print_endline (string_of_pitch A ^ string_of_accidental Sharp)

let rec string_of_song song = match song with
| Note (p, a, flop) ->
    string_of_pitch p ^ string_of_accidental a ^ " " ^ string_of_song flop
| End -> ""

let song = Note (A, Natural, Note (E, Sharp, Note (F, Flat, Note (C, Natural, End))))
let _ = print_endline (string_of_song song)


type song =
| Note of pitch * accidental * song
| End

type 'a list =
| Cons of 'a * 'a list
| Nil

type song = (pitch * accidental) list
