%{
  open Utils
%}

%token <int> NUM
%token <string> VAR
%token IF THEN ELSE LET IN FUN TRUE FALSE UNIT
%token REC
%token ADD SUB MUL DIV MOD
%token LT LTE GT GTE EQ NEQ AND OR ARROW
%token EOF
%token INT BOOL
%token COLON LPAREN RPAREN

%start <prog> prog
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

(* Function arguments *)
arg_list:
  | LPAREN VAR COLON ty RPAREN { [($2, $4)] }
  | arg_list LPAREN VAR COLON ty RPAREN { ($3, $5) :: $1 }
  | /* empty */ { [] }

(* Types *)
ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | ty ARROW ty { FunTy ($1, $3) }
  | LPAREN ty RPAREN { $2 }

(* Expressions *)
expr:
  | IF expr THEN expr ELSE expr { SIf($2, $4, $6) }
  | LET VAR EQ expr IN expr {
      SLet {
        is_rec = false;
        name = $2;
        args = [];
        ty = UnitTy;
        value = $4;
        body = $6;
      }
    }
  | LET REC VAR EQ FUN VAR ARROW expr IN expr {
      SLet {
        is_rec = true;
        name = $3;
        args = [($6, UnitTy)];
        ty = UnitTy;
        value = $8;
        body = $10;
      }
    }
  | expr2 { $1 }

expr2:
  | expr2 expr3 { SApp($1, $2) }
  | expr2 ADD expr2 { SBop(Add, $1, $3) }
  | expr2 SUB expr2 { SBop(Sub, $1, $3) }
  | expr2 MUL expr2 { SBop(Mul, $1, $3) }
  | expr2 DIV expr2 { SBop(Div, $1, $3) }
  | expr2 MOD expr2 { SBop(Mod, $1, $3) }
  | expr2 LT expr2 { SBop(Lt, $1, $3) }
  | expr2 LTE expr2 { SBop(Lte, $1, $3) }
  | expr2 GT expr2 { SBop(Gt, $1, $3) }
  | expr2 GTE expr2 { SBop(Gte, $1, $3) }
  | expr2 EQ expr2 { SBop(Eq, $1, $3) }
  | expr2 NEQ expr2 { SBop(Neq, $1, $3) }
  | expr2 AND expr2 { SBop(And, $1, $3) }
  | expr2 OR expr2 { SBop(Or, $1, $3) }
  | expr3 { $1 }

expr3:
  | LPAREN expr RPAREN { $2 }
  | TRUE { STrue }
  | FALSE { SFalse }
  | UNIT { SUnit }
  | NUM { SNum($1) }
  | VAR { SVar($1) }

(* Top-level let definitions *)
toplet_list:
  | toplet { [$1] }
  | toplet_list toplet { $1 @ [$2] }

(* A single let definition *)
toplet:
  | LET VAR arg_list COLON ty EQ expr {
      SLet {
        is_rec = false;
        name = $2;
        args = $3;
        ty = $5;
        value = $6;
        body = SUnit
      }
    }
  | LET REC VAR arg_list COLON ty EQ expr {
      SLet {
        is_rec = true;
        name = $3;
        args = $4;
        ty = $6;
        value = $7;
        body = SUnit
      }
    }

