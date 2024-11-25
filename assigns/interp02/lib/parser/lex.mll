{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+ 
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
 | whitespace { read lexbuf }
 | "let"{ LET }
 | "rec" { REC }
 | "in"  { IN }
 | "if"  { IF }
 | "then"  { THEN }
 | "else"  { ELSE }
 | "fun"  { FUN }
 | "assert" { ASSERT }
 | "true"  { TRUE }
 | "false" { FALSE }
 | "int" { INT }
 | "bool" { BOOL }
 | "unit"  { UNIT }
 | "mod" { MOD }
 | "(" { LPAREN }
 | ")" { RPAREN }
 | ":" { COLON }
 | "->"  { ARROW }
 | "=" { EQUALS }
 | "+" { PLUS }
 | "-" { MINUS }
 | "*"  { TIMES }
 | "/" { DIV }
 | "<" { LT }
 | "<="  { LTE }
 | ">" { GT }
 | ">="  { GTE }
 | "=="  { EQ }
 | "<>"  { NEQ }
 | "&&" { AND }
 | "||"  { OR }
 | "()"  { UNIT_VAL } | var { VAR(Lexing.lexeme lexbuf) }
 | num { NUM(int_of_string (Lexing.lexeme lexbuf)) }
 | eof { EOF }
