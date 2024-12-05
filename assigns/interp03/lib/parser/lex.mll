{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let int = '-'? ['0'-'9']+
let float = '-'? ['0'-'9']+ '.' ['0'-'9']*
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let tvar = '\'' ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "let" { LET }
  | "rec" { REC }
  | "=" { EQUALS }
  | "in" { IN }
  | ":" { COLON }
  | "fun" { FUN }
  | "match" { MATCH }
  | "with" { WITH }
  | "|" { ALT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | ";" { SEMICOLON }
  | "unit" { TUNIT }
  | "int" { TINT }
  | "float" { TFLOAT }
  | "bool" { TBOOL }
  | "list" { TLIST }
  | "->" { ARROW }
  | "()" { UNIT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "mod" { MOD }
  | "+." { ADDF }
  | "-." { SUBF }
  | "*." { MULF }
  | "/." { DIVF }
  | "**" { POW }
  | "::" { CONS }
  | "@" { CONCATL }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "," { COMMA }
  | "assert" { ASSERT }
  | "option" { OPTION }
  | "Some" { SOME }
  | "None" { NONE }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | tvar { TVAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
  | "(*" { read_comment 0 lexbuf }
  | _ { failwith ("Unknown token: " ^ Lexing.lexeme lexbuf) }
and read_comment depth =
  parse
  | "(*" { read_comment (depth + 1) lexbuf }
  | "*)"
    { if depth = 0
      then read lexbuf
      else read_comment (depth - 1) lexbuf
    }
  | [^ '(' '*']+ { read_comment depth lexbuf }
  | [ '(' '*' ] { read_comment depth lexbuf }
  | _ { failwith "Comment not closed" }
