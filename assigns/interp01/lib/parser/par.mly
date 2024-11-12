%{
open Utils

let rec mk_app e = function
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token UNIT
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token EQ
%token NEQ
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token LET
%token REC            (* Token for "rec" *)
%token IN
%token FUN
%token ARROW

%token EOF

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LET; REC; x = VAR; EQ; e1 = expr; IN; e2 = expr { LetRec (x, e1, e2) }  (* Rule for recursive let *)
  | LET;
