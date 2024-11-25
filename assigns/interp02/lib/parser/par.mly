%{
  open Utils
%}

%token <int> NUM
%token <string> VAR
%token IF THEN ELSE LET IN FUN TRUE FALSE UNIT
%token REC ASSERT
%token ADD SUB MUL DIV MOD
%token LT LTE GT GTE EQ NEQ AND OR ARROW
%token EOF
%token INT BOOL
%token COLON LPAREN RPAREN

%start <sfexpr list> prog
%type <sfexpr> expr

%left OR
%left AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD
%right ARROW

%%

prog:
  | toplet_list EOF { $1 }

toplet_list:
  | /* empty */ { [] }
  | toplet toplet_list { $1 :: $2 }

toplet:
  | LET VAR arg_list COLON ty EQ expr {
      SLet { is_rec = false; name = $2; args = $3; ty = $5; value = $7; body = SUnit }
    }
  | LET REC VAR arg_list COLON ty EQ expr {
      SLet { is_rec = true; name = $3; args = $4; ty = $6; value = $8; body = SUnit }
    }

arg_list:
  | /* empty */ { [] }
  | arg arg_list { $1 :: $2 }

arg:
  | LPAREN VAR COLON ty RPAREN { ($2, $4) }

ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | ty ARROW ty { FunTy ($1, $3) }
  | LPAREN ty RPAREN { $2 }

expr:
  | LET VAR arg_list COLON ty EQ expr IN expr {
      SLet { is_rec = false; name = $2; args = $3; ty = $5; value = $7; body = $9 }
    }
  | LET REC VAR arg_list COLON ty EQ expr IN expr {
      SLet { is_rec = true; name = $3; args = $4; ty = $6; value = $8; body = $10 }
    }
  | IF expr THEN expr ELSE expr { SIf ($2, $4, $6) }
  | FUN arg_list ARROW expr {
      List.fold_right (fun (x, ty) acc -> SFun { arg = (x, ty); args = []; body = acc }) $2 $4
    }
  | expr2 { $1 }

expr2:
  | expr2 ADD expr3 { SBop (Add, $1, $3) }
  | expr2 SUB expr3 { SBop (Sub, $1, $3) }
  | expr2 MUL expr3 { SBop (Mul, $1, $3) }
  | expr2 DIV expr3 { SBop (Div, $1, $3) }
  | expr2 MOD expr3 { SBop (Mod, $1, $3) }
  | expr2 LT expr3 { SBop (Lt, $1, $3) }
  | expr2 LTE expr3 { SBop (Lte, $1, $3) }
  | expr2 GT expr3 { SBop (Gt, $1, $3) }
  | expr2 GTE expr3 { SBop (Gte, $1, $3) }
  | expr2 EQ expr3 { SBop (Eq, $1, $3) }
  | expr2 NEQ expr3 { SBop (Neq, $1, $3) }
  | expr2 AND expr3 { SBop (And, $1, $3) }
  | expr2 OR expr3 { SBop (Or, $1, $3) }
  | ASSERT expr3 { SAssert $2 }
  | expr3 { $1 }

expr3:
  | NUM { SNum $1 }
  | VAR { SVar $1 }
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
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


