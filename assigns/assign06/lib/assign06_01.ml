open Utils  (* Import functions and types from utils.ml *)

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

(* Test function to check lexing behavior *)
let () =
  match lex "2 3 + 5 < ?" with
  | Some tokens -> 
      my_iter (function
        | Utils.TNum n -> print_endline ("TNum " ^ string_of_int n)
        | Utils.TAdd -> print_endline "TAdd"
        | Utils.TLt -> print_endline "TLt"
        | Utils.TIte -> print_endline "TIte") tokens
  | None -> print_endline "Lexing failed: Invalid token"
