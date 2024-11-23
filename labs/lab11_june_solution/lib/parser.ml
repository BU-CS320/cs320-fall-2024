
let parse c =
  Par.prog Lex.read (Lexing.from_channel c)