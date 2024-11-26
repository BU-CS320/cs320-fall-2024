%{
open Utils

let mk_func ty args body =
  let body =
    match ty with
    | None -> body
    | Some ty -> Annot (body, ty)
  in
  List.fold_right
    (fun (x, ty) acc -> Fun (x, ty, acc))
    args
    body

let mk_list es =
  List.fold_right
    (fun x acc -> Bop (Cons, x, acc))
    es
    Nil
%}

%token EOF
%token <int> INT
%token <float> FLOAT
%token <string> VAR

%token LET "let"
%token REC "rec"
%token EQUALS "="
%token IN "in"
%token COLON ":"

%token FUN "fun"

%token MATCH "match"
%token WITH "with"
%token ALT "|"

%token IF "if"
%token THEN "then"
%token ELSE "else"

%token LPAREN "("
%token RPAREN ")"
%token LBRACKET "["
%token RBRACKET "]"
%token SEMICOLON ";"

%token TUNIT "unit"
%token TINT "int"
%token TFLOAT "float"
%token TBOOL "bool"
%token TLIST "list"
%token <string> TVAR
%token ARROW "->"

%token UNIT "()"
%token TRUE "true"
%token FALSE "false"

%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token MOD "mod"
%token ADDF "+."
%token SUBF "-."
%token MULF "*."
%token DIVF "/."
%token POW "**"
%token CONS "::"
%token CONCATL "@"
%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token NEQ "<>"
%token AND "&&"
%token OR "||"
%token COMMA ","

%token OPTION "option"
%token SOME "Some"
%token NONE "None"

%token ASSERT "assert"

%nonassoc TLIST
%nonassoc OPTION
%right ARROW

%nonassoc COMMA
%right OR
%right AND
%left LT LTE GT GTE EQUALS NEQ
%right CONCATL
%right CONS
%left ADD ADDF SUB SUBF
%left MUL MULF DIV DIVF MOD
%left POW

%start <Utils.prog> prog

%%

prog:
  | ls = toplet* EOF { ls }

toplet:
  | "let" r = "rec"?
    name = VAR args = arg* ty = annot?
    "=" value = expr
    { { is_rec = Option.is_some r
      ; name
      ; value = mk_func ty args value
      }
    }

annot:
  | ":" ty = ty { ty }

ty:
  | "unit" { TUnit }
  | "int" { TInt }
  | "float" { TFloat }
  | "bool" { TBool }
  | ty = ty "list" { TList ty }
  | ty = ty "option" { TOption ty }
  | x = TVAR { TVar x }
  | t1 = ty "->" t2 = ty { TFun (t1, t2) }
  | t1 = ty "*" t2 = ty { TPair (t1, t2) }
  | "(" ty = ty ")" { ty }

arg:
  | x = VAR { (x, None) }
  | "(" x = VAR ty = annot ")" { (x, Some ty) }

expr:
  | "let" r = "rec"?
    name = VAR args = arg* ty = annot?
    "=" value = expr "in" body = expr
    { Let
      { is_rec = Option.is_some r
      ; name
      ; value = mk_func ty args value
      ; body
      }
    }
  | "fun" args = arg* "->" body = expr
    { mk_func None args body }
  | "if" e1 = expr "then" e2 = expr "else" e3 = expr
    { If (e1, e2, e3) }
  | "match" matched = expr "with"
    "|" hd_name = VAR "::" tl_name = VAR "->"
        cons_case = expr
    "|" "[" "]" "->" nil_case = expr
    { ListMatch {matched;nil_case;hd_name;tl_name;cons_case} }
  | "match" matched = expr "with"
    "|" fst_name = VAR "," snd_name = VAR "->" case = expr
    { PairMatch {matched;fst_name;snd_name;case} }
  | "match" matched = expr "with"
    "|" "Some" some_name = VAR "->" some_case = expr
    "|" "None" "->" none_case = expr
    { OptMatch {matched;some_name;some_case;none_case} }
  | e = expr2 { e }

%inline bop:
  | "+" { Add }
  | "-" { Sub }
  | "*" { Mul }
  | "/" { Div }
  | "mod" { Mod }
  | "+." { AddF }
  | "-." { SubF }
  | "*." { MulF }
  | "/." { DivF }
  | "**" { PowF }
  | "::" { Cons }
  | "@" { Concat }
  | "<" { Lt }
  | "<=" { Lte }
  | ">" { Gt }
  | ">=" { Gte }
  | "=" { Eq }
  | "<>" { Neq }
  | "&&" { And }
  | "||" { Or }
  | "," { Comma }

expr2:
  | e1 = expr2 op = bop e2 = expr2 { Bop (op, e1, e2) }
  | "assert" e = expr3 { Assert e }
  | "Some" e = expr3 { ESome e }
  | e = expr3 es = expr3*
    { List.fold_left (fun acc x -> App (acc, x)) e es }

list_item:
  | ";" e = expr { e }

expr3:
  | "()" { Unit }
  | "true" { True }
  | "false" { False }
  | "[" "]" { Nil }
  | "None" { ENone }
  | "[" e = expr es = list_item* "]"
    { mk_list (e :: es) }
  | n = INT { Int n }
  | n = FLOAT { Float n }
  | x = VAR { Var x }
  | "(" e = expr ":" ty = ty ")" { Annot (e, ty) }
  | "(" e = expr ")" { e }
