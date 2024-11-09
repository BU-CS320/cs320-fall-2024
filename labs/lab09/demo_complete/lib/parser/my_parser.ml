let parse s =
  try Some (Par.prog Lex.read (Lexing.from_string s))
  with _ -> None
