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
%token <int> NUM
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token PLUS "+"
%token TIMES "*"
%token MINUS "-"
%token TRUE "true"
%token FALSE "false"

%left EQUALS
%left PLUS MINUS
%left TIMES

%start <Utils.prog> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | "let" x = VAR "=" e1 = expr "in" e2 = expr { Let(x, e1, e2) }
  | "if" e1 = expr "then" e2 = expr "else" e3 = expr { If (e1, e2, e3) }
  | "fun" x = VAR "->" e = expr { Fun(x, e) }
  | e = expr2 { e }

expr2:
  | e1 = expr2 "+" e2 = expr2 { Add (e1, e2) }
  | e1 = expr2 "*" e2 = expr2 { Mul (e1, e2) }
  | e1 = expr2 "-" e2 = expr2 { Sub (e1, e2) }
  | e1 = expr2 "=" e2 = expr2 { Eq (e1, e2) }
  | e = expr3 es = expr3*
    { List.fold_left (fun e1 e2 -> App (e1, e2)) e es }

expr3:
  | x = VAR { Var x }
  | n = NUM { Num n }
  | "true" { True }
  | "false" { False }
  | "(" e = expr ")" { e }
