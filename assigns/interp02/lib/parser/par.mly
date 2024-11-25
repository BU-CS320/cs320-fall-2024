%{
open Utils

let make_toplet ~is_rec name args ty value = 
  { is_rec; name; args; ty; value }
%}

%token <int> NUM
%token <string> VAR
%token LET REC IN
%token IF THEN ELSE
%token FUN ARROW
%token ASSERT
%token TRUE FALSE
%token INT BOOL UNIT UNIT_VAL
%token LPAREN RPAREN
%token COLON EQUALS
%token PLUS MINUS TIMES DIV MOD
%token LT LTE GT GTE EQ NEQ
%token AND OR
%token EOF

%nonassoc IN
%nonassoc ELSE
%right ARROW
%right OR
%right AND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc APP
%nonassoc LPAREN

%start <Utils.prog> prog

%%

prog:
  | ts = list(toplet) EOF { ts }

toplet:
  | LET x = VAR args = list(arg) COLON t = ty EQUALS e = expr
    { make_toplet ~is_rec:false x args t e }
  | LET REC x = VAR arg = arg args = list(arg) COLON t = ty EQUALS e = expr
    { make_toplet ~is_rec:true x (arg :: args) t e }

arg:
  | LPAREN x = VAR COLON t = ty RPAREN { (x, t) }

ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | t1 = ty ARROW t2 = ty { FunTy(t1, t2) }
  | LPAREN t = ty RPAREN { t }

expr3:
  | UNIT_VAL { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n = NUM { SNum n }
  | x = VAR { SVar x }
  | LPAREN e = expr RPAREN { e }

expr2:
  | ASSERT LPAREN e = expr RPAREN { SAssert(e) }
  | e1 = expr2 e2 = expr3 { SApp(e1, e2) }
  | e = expr3 { e }

expr1:
  | e1 = expr1 op = binop e2 = expr2 { SBop(op, e1, e2) }
  | e = expr2 { e }

expr:
  | FUN args = nonempty_list(arg) ARROW body = expr
    { List.fold_right (fun (x,t) acc -> SFun{arg=(x,t); args=[]; body=acc}) args body }
  | LET x = VAR args = list(arg) COLON t = ty EQUALS e1 = expr IN e2 = expr
    { SLet{is_rec=false; name=x; args; ty=t; value=e1; body=e2} }
  | LET REC x = VAR arg = arg args = list(arg) COLON t = ty EQUALS e1 = expr IN e2 = expr
    { SLet{is_rec=true; name=x; args=(arg::args); ty=t; value=e1; body=e2} }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { SIf(e1, e2, e3) }
  | e = expr1 { e }

%inline binop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIV { Div }
  | MO
