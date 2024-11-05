{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+

rule read =
  parse
  | whitespace { read lexbuf }
  | eof { EOF }
