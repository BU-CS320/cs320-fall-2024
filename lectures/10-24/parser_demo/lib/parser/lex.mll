{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
