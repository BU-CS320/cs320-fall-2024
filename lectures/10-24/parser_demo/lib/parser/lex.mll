{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
(*
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
*)

rule read =
  parse
  | whitespace { read lexbuf }
  | eof { EOF }
