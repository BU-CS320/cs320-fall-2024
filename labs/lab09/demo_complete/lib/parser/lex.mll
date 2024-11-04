{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
let num = '-'? ['0'-'9']+

rule read =
  parse
  | "true" { TRUE }
  | "false" { FALSE }
  | "+" { ADD }
  | "=" { EQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | whitespace { read lexbuf }
  | eof { EOF }
