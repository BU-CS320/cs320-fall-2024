{
open Par  
}

let ws  = [' ' '\n' '\t' '\r']+
let int = ['0'-'9']+
let id  = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']*

rule read =
  parse
  | "int" { INT }
  | "bool" { BOOL }
  | ":" { ANN } 
  | int { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | "true"  { BOOLEAN true  }
  | "false" { BOOLEAN false }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  | "<" { LT }
  | ">" { GT }
  | "<=" { LTE }
  | ">=" { GTE }
  | "==" { EQEQ }
  | "!=" { NEQ }
  | "fun" { FUN }
  | "->" { RIGHTARROW }
  | "let" { LET }
  | "rec" { REC }
  | "=" { EQ }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | ws { read lexbuf }
  | eof { EOF }
   