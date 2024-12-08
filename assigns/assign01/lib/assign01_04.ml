(* lib/assign01_04.ml *)

open Assign01_03  (* Import the nth function *)

let to_string s =
  let rec get_elements s acc =
    let next = nth s (List.length acc) in
    if next = 0 then acc else get_elements s (acc @ [next])
  in
  let elements = get_elements s [] in
  "[" ^ (String.concat "; " (List.map string_of_int elements)) ^ "]"

(* Example usage and test cases:
let _ =
  let _ = assert (to_string 1 = "[]") in
  let _ = assert (to_string 8 = "[3]") in
  let _ = assert (to_string 24 = "[3; 1]") in
  let _ = assert (to_string 1058400 = "[5; 3; 2; 2]") in
  ()
*)
