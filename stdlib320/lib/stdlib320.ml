include Stdlib

let gensym =
  let count = ref 0 in
  fun () ->
    let out = "$x" ^ string_of_int !count in
    let _ = count := !count + 1 in
    out

let read_file filename =
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines ^ "\n" ^ input_line chan
    done; !lines
  with End_of_file ->
    close_in chan;
    !lines

let print_byte b = output_byte stdout b
let read_byte () = flush stdout; (input_byte stdin)

module List = struct
  include List
  (****************************************************************)
  (* Taken from the standard library for OCaml 5.3:               *)
  (*   https://github.com/ocaml/ocaml/blob/trunk/stdlib/list.ml   *)
  (****************************************************************)
  let take n l =
    let[@tail_mod_cons] rec aux n l =
      match n, l with
      | 0, _ | _, [] -> []
      | n, x::l -> x::aux (n - 1) l
    in
    if n < 0 then invalid_arg "List.take";
    aux n l

  let drop n l =
    let rec aux i = function
      | _x::l when i < n -> aux (i + 1) l
      | rest -> rest
    in
    if n < 0 then invalid_arg "List.drop";
    aux 0 l

  let take_while p l =
    let[@tail_mod_cons] rec aux = function
      | x::l when p x -> x::aux l
      | _rest -> []
    in
    aux l

  let rec drop_while p = function
    | x::l when p x -> drop_while p l
    | rest -> rest
end

module Env = struct
  include Map.Make(String)
  let of_list l = of_seq (List.to_seq l)
  let to_list e = List.of_seq (to_seq e)
end

type 'a env = 'a Env.t

module Option = struct
  include Option
  let default none x = Option.value x ~default:none
end

module Result = struct
  include Result
  let default err x = Result.value x ~default:err
end

module String = struct
  include String
  let to_list s = List.of_seq (String.to_seq s)
  let of_list cs = String.of_seq (List.to_seq cs)
end
