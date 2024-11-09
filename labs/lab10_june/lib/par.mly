%{
open Syntax
%}

// constants
%token <int> INT
%token <bool> BOOL
%token <string> ID

// delimiters
%token LPAREN
%token RPAREN

// integer
%token ADD
%token SUB
%token MUL
%token DIV

// boolean
%token AND
%token OR
%token NOT

// compare
%token LT
%token GT
%token LTE
%token GTE
%token EQEQ
%token NEQ

// end of input
%token EOF

// precedence and associativity
%left OR
%left AND
%right EQEQ NEQ LT GT LTE GTE
%left ADD SUB
%left MUL DIV

%token FUN
%token RIGHTARROW
%token LET
%token EQ
%token IN
%token IF
%token THEN
%token ELSE

%start <Syntax.expr> prog

%%

let parens(P) ==
  | LPAREN; ~ = P; RPAREN; <>

let infix ==
  // integer
  | MUL; { Mul }
  | DIV; { Div }
  | ADD; { Add }
  | SUB; { Sub }
  // boolean
  | AND; { And }
  | OR;  { Or  }
  // compare
  | LT;  { Lt  }
  | GT;  { Gt  }
  | LTE; { Lte }
  | GTE; { Gte }
  | EQEQ;  { Eq  }
  | NEQ; { Neq }

let fun_expr :=
  | FUN; xs = ID+; RIGHTARROW; m = expr;
    { List.fold_right (fun x acc -> Fun (x, acc)) xs m }

let let_expr :=
  | LET; x = ID; EQ; m = expr; IN; n = expr;
    { Let (x, m, n) }

let ifte_expr :=
  | IF; m = expr; THEN; n1 = expr; ELSE; n2 = expr;
    { Ifte (m, n1, n2) }

let expr0 :=
  | i = INT; { Int i }
  | b = BOOL; { Bool b }
  | NOT; m = expr0; { UnaryOp (Not, m) }
  | x = ID; { Var x }
  | ~ = parens(expr); <>

let expr1 :=
  | ms = expr0+;
    { match ms with
      | [] -> failwith ""
      | m :: ms -> List.fold_left (fun acc m -> App (acc, m)) m ms }

let expr2 :=
  | SUB; m = expr2; { UnaryOp (Neg, m) }
  | ~ = expr1; <>

let expr3 :=
  | m = expr3; op = infix; n = expr3; { BinaryOp (op, m, n) }
  | ~ = expr2; <>

let expr :=
  | ~ = fun_expr; <>
  | ~ = let_expr; <>
  | ~ = ifte_expr; <>
  | ~ = expr3; <>

let prog :=
  | ~ = expr; EOF; <>
