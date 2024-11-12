{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
let var = ['a'-'z']+
let num = '-'? ['0'-'9']+

rule read =
  parse
  | "int" { INT }
  | "bool" { BOOL }
  | ":" { COLON }
  | "fun" { FUN }
  | "let" { LET }
  | "rec" { REC }
  | "=" { EQUALS }
  | "in" { IN }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "true" { TRUE }
  | "false" { FALSE }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
