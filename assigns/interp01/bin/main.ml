open Utils
open Lib

let () =
  let input =
    if Array.length Sys.argv > 1 then
      let filename = Sys.argv.(1) in
      try Stdlib320.read_file filename with _ ->
        failwith "Error: Could not read \'" ^ filename ^ "\'"
    else
      failwith "Error: No file given"
  in
  match interp input with
  | Ok v -> print_endline (string_of_value v)
  | Error e -> failwith ("Error: " ^ err_msg e)
