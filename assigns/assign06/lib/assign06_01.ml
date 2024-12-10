open Utils  

(* Manually implement the `iter` function *)
let rec my_iter f = function
  | [] -> ()
  | x :: xs -> f x; my_iter f xs

(* Implement the lexer function *)
let lex (s: string) : tok list option =
  let words = Utils.split s in
  let rec aux acc = function
    | [] -> Some (List.rev acc)  (* Successfully parsed all tokens *)
    | w :: ws -> (
        match Utils.tok_of_string_opt w with
        | Some token -> aux (token :: acc) ws  (* Add token to the list *)
        | None -> None  (* Invalid token, return None *)
      )
  in
  aux [] words

