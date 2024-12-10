%{
  open Utils
%}

%token <int> NUM
%token <string> VAR
%token IF THEN ELSE LET IN FUN ARROW
%token TRUE FALSE LPAREN RPAREN
%token ADD SUB MUL DIV MOD LT LTE GT GTE EQ NEQ AND OR
%token EOF

%start <Utils.prog> prog
%type <Utils.expr> expr

%%

prog:
  | expr EOF { $1 }

expr:
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | LET VAR EQ expr IN expr { Let ($2, $4, $6) }
  | FUN VAR ARROW expr { Fun ($2, $4) }
  | logical_expr { $1 }

logical_expr:
  | logical_expr OR and_expr { Bop (Or, $1, $3) }
  | and_expr { $1 }

and_expr:
  | and_expr AND comparison_expr { Bop (And, $1, $3) }
  | comparison_expr { $1 }

comparison_expr:
  | comparison_expr LT add_expr { Bop (Lt, $1, $3) }
  | comparison_expr LTE add_expr { Bop (Lte, $1, $3) }
  | comparison_expr GT add_expr { Bop (Gt, $1, $3) }
  | comparison_expr GTE add_expr { Bop (Gte, $1, $3) }
  | comparison_expr EQ add_expr { Bop (Eq, $1, $3) }
  | comparison_expr NEQ add_expr { Bop (Neq, $1, $3) }
  | add_expr { $1 }

add_expr:
  | add_expr ADD mul_expr { Bop (Add, $1, $3) }
  | add_expr SUB mul_expr { Bop (Sub, $1, $3) }
  | mul_expr { $1 }

mul_expr:
  | mul_expr MUL primary_expr { Bop (Mul, $1, $3) }
  | mul_expr DIV primary_expr { Bop (Div, $1, $3) }
  | mul_expr MOD primary_expr { Bop (Mod, $1, $3) }
  | primary_expr { $1 }

primary_expr:
  | primary_expr_atomic primary_expr_seq { List.fold_left (fun acc e -> App (acc, e)) $1 $2 }
  | primary_expr_atomic { $1 }

primary_expr_atomic:
  | LPAREN expr RPAREN { $2 }
  | TRUE { True }
  | FALSE { False }
  | NUM { Num $1 }
  | VAR { Var $1 }
  | LPAREN RPAREN { Unit }

primary_expr_seq:
  | primary_expr_atomic primary_expr_seq { $1 :: $2 }
  | primary_expr_atomic { [$1] }

