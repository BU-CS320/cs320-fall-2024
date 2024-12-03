%{
open Utils
%}

%token <string> VAR
%token <int> NUM

%token FUN "fun"
%token ARROW "->"
%token LPAREN "("
%token RPAREN ")"
%token LET "let"
%token REC "rec"
%token EQUALS "="
%token IN "in"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token PLUS "+"
%token EOF

%left EQUALS
%left PLUS

%start <Utils.prog> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | "let" x = VAR "=" e1 = expr "in" e2 = expr { Let(x, e1, e2) }
  | "let" "rec" f = VAR x = VAR "=" e1 = expr "in" e2 = expr { LetRec(f, x, e1, e2) }
  | "if" e1 = expr "then" e2 = expr "else" e3 = expr { If (e1, e2, e3) }
  | "fun" x = VAR "->" e = expr { Fun(x, e) }
  | e = expr2 { e }

expr2:
  | e1 = expr2 "+" e2 = expr2 { Add (e1, e2) }
  | e1 = expr2 "=" e2 = expr2 { Eq (e1, e2) }
  | e = expr3 es = expr3*
    { List.fold_left (fun e1 e2 -> App (e1, e2)) e es }

expr3:
  | x = VAR { Var x }
  | n = NUM { Num n }
  | "(" e = expr ")" { e }

