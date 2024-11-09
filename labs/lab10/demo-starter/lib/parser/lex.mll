{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
let var = ['a'-'z']+

rule read =
  parse
  | "fun" { FUN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
