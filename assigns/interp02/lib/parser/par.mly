%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token UNIT TRUE FALSE BOOL INT  (* 添加 BOOL 和 INT *)
%token LPAREN RPAREN
%token ADD SUB MUL DIV MOD
%token LT LTE GT GTE EQ NEQ
%token AND OR
%token IF THEN ELSE
%token LET REC IN FUN
%token COLON ARROW ASSERT
%token EOF

%start <prog> prog

%%

prog:
  | toplets EOF { $1 }

toplets:
  | /* empty */ { [] }
  | toplet toplets { $1 :: $2 }

toplet:
  | LET VAR args COLON ty EQ expr { { is_rec = false; name = $2; args = $3; ty = $5; value = $7 } }
  | LET REC VAR args COLON ty EQ expr { { is_rec = true; name = $3; args = $4; ty = $6; value = $8 } }

args:
  | /* empty */ { [] }
  | arg args { $1 :: $2 }

arg:
  | LPAREN VAR COLON ty RPAREN { ($2, $4) }

ty:
  | INT { IntTy }            (* 使用 INT token *)
  | BOOL { BoolTy }          (* 使用 BOOL token *)
  | UNIT { UnitTy }
  | ty ARROW ty { FunTy ($1, $3) }
  | LPAREN ty RPAREN { $2 }

expr:
  | LET VAR args COLON ty EQ expr IN expr { Let { is_rec = false; name = $2; ty = $5; value = $7; body = $9 } }
  | LET REC VAR args COLON ty EQ expr IN expr { Let { is_rec = true; name = $3; ty = $6; value = $8; body = $10 } }
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | FUN args ARROW expr { List.fold_right (fun (x, ty) acc -> Fun (x, ty, acc)) $2 $4 }
  | expr2 { $1 }

expr2:
  | expr2 bop expr2 { Bop ($2, $1, $3) }
  | ASSERT expr3 { Assert $2 }
  | expr3 { $1 }

expr3:
  | NUM { Num $1 }
  | VAR { Var $1 }
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | LPAREN expr RPAREN { $2 }

bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
