type op = Add | Sub | Mul | Div

type expr =
  | Num of int
  | Bop of op * expr * expr

let code_of_op = function
  | Add -> 0
  | Sub -> 1
  | Mul -> 2
  | Div -> 3

let push = 4
let emit = output_byte stdout

let emit_int n =
  emit push; output_binary_int stdout n

let compile =
  let rec go = function
    | Num n -> emit_int n
    | Bop(op, e1, e2) -> go e2; go e1; emit (code_of_op op)
  in go

let compile_str s =
  let rec go = function
  | Num n -> ["PUSH " ^ string_of_int n]
  | Bop (op, e1, e2) ->
    let op =
      match op with
      | Add -> "ADD"
      | Sub -> "SUB"
      | Mul -> "MUL"
      | Div -> "DIV"
    in go e2 @ go e1 @ [op]
  in String.concat "\n" (go s)

let test =
  Bop
    ( Add
    , Bop (Mul, Num 3, Num 4)
    , Bop (Mul, Num 2, Num (-1))
    )

let () = compile test
(* let () = print_endline (compile_str test) *)
