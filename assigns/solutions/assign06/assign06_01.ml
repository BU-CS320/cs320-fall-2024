open Utils

let cons_opt x = Option.map (fun xs -> x :: xs)

let list_opt_of_opt_list =
  let rec go = function
    | [] -> Some []
    | None :: _ -> None
    | Some x :: l -> cons_opt x (go l)
  in go

let lex s =
  s
  |> split
  |> List.map tok_of_string_opt
  |> list_opt_of_opt_list
