%{
open Utils
%}

%token EOF
%token FUN "fun"
%token ARROW "->"
%token LPAREN "("
%token RPAREN ")"
%token LET "let"
%token EQUALS "="
%token IN "in"
%token <string> VAR

%start <Utils.prog> prog

%%

prog:
  | lets = toplet* EOF { lets }

toplet:
  | "let" x = VAR "=" e = expr { (x, e) }

expr:
  | "let" x = VAR "=" e1 = expr "in" e2 = expr { Let(x, e1, e2) }
  | "fun" x = VAR "->" e = expr { Fun(x, e) }
  | e = expr2 es = expr2*
    { List.fold_left (fun e1 e2 -> App (e1, e2)) e es }

expr2:
  | x = VAR { Var x }
  | "(" e = expr ")" { e }
