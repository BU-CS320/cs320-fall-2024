open Assign01_02
open Assign01_03

let length seq =
  let rec go k count =
    if seq mod k <> 0
    then count
    else go (next_prime k) (count + 1)
  in go 2 0

let to_string seq =
  let n = length seq in
  let rec go k =
    if k < n - 1
    then string_of_int (nth seq k) ^ "; " ^ go (k + 1)
    else string_of_int (nth seq k)
  in
  let inner =
    if n = 0
    then ""
    else go 0
  in "[" ^ inner ^ "]"
