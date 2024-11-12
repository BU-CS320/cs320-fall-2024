%{
open Utils
%}

%token EOF
%token FUN "fun"
%token ARROW "->"
%token LPAREN "("
%token RPAREN ")"
%token LET "let"
%token REC "rec"
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

%token COLON ":"
%token INT "int"
%token BOOL "bool"

%right ARROW
%left EQUALS
%left PLUS MINUS
%left TIMES

%start <Utils.prog> prog

%%

prog:
  | e = expr EOF { e }

ty:
  | "int" { IntTy }
  | "bool" { BoolTy }
  | t1 = ty "->" t2 = ty { FunTy (t1, t2) }
  | "(" ty = ty ")" { ty }

expr:
  | "let" x = VAR ":" ty = ty "=" e1 = expr "in" e2 = expr { Let(x, ty, e1, e2) }
  | "let" "rec" f = VAR
    "(" x = VAR ":" ty_arg = ty ")"
    ":" ty_val = ty "=" e1 = expr "in" e2 = expr
    { LetRec(f, x, ty_arg, ty_val, e1, e2) }
  | "if" e1 = expr "then" e2 = expr "else" e3 = expr { If (e1, e2, e3) }
  | "fun" "(" x = VAR ":" ty = ty ")" "->" e = expr { Fun(x, ty, e) }
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
