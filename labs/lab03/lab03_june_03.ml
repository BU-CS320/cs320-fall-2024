
type pitch = A | B | C | D | E | F | G

type accidental = Natural | Flat | Sharp

type song =
| Note of pitch * accidental * song
| End

let song = Note (A, Natural, Note (C, Flat, End))

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
| Sharp -> "#"
| Flat -> "b"

let _ = print_endline (
    string_of_pitch A ^ string_of_accidental Sharp
)

let rec string_of_song song = match song with
| Note (pitch, acc, song') ->
    string_of_pitch pitch ^ string_of_accidental acc ^ " " ^ string_of_song song'
| End -> ""


let song = Note (C, Natural, Note (G, Flat, Note (A, Sharp, Note (E, Natural, End))))

let _ = print_endline (string_of_song song)


type song =
| Note of pitch * accidental * song
| End

type 'a list =
| Cons of 'a * 'a list
| Nil

type 'a list =
| (::) of 'a * 'a list
| []

let zs = Cons (1, Cons (2, Cons (3, Nil)))

let xs = 1 :: 2 :: 3 :: []
let ys = [1;2;3]
let _ = assert (xs = ys)


type song = (pitch * accidental) list

