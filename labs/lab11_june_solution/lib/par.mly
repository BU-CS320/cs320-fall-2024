%{
open Syntax
%}

// types
%token INT
%token BOOL
%token ANN

// constants
%token <int> INTEGER
%token <bool> BOOLEAN
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
%right RIGHTARROW
%left OR
%left AND
%right EQEQ NEQ LT GT LTE GTE
%left ADD SUB
%left MUL DIV

%token FUN
%token RIGHTARROW
%token LET
%token REC
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

let ty0 :=
  | INT; { TInt }
  | BOOL; { TBool }

let ty :=
  | a = ty; RIGHTARROW; b = ty; { TArrow (a, b) }
  | ~ = ty0; <>

let ann :=
  | ANN; t = ty; { Some t }
  | { None }

let expr0 :=
  | i = INTEGER; { EInt i }
  | b = BOOLEAN; { EBool b }
  | x = ID; { Var x }
  | NOT; m = expr0; { UnaryOp (Not, m) }
  | LPAREN; m = expr; opt = ann; RPAREN;
    { match opt with
      | Some t -> Ann (m, t)
      | None -> m }

let expr1 :=
  | ms = expr0+;
    { match ms with
      | [] -> assert false
      | f :: ms ->
        List.fold_left (fun acc m -> App (acc, m))
          f ms }

let expr2 :=
  | SUB; m = expr2; { UnaryOp (Neg, m) }
  | ~ = expr1; <>

let expr3 :=
  | m = expr3; op = infix; n = expr3; { BinaryOp (op, m, n) }
  | ~ = expr2; <>

let arg :=
  | LPAREN; x = ID; ANN; t = ty; RPAREN; { (x, t) }

let fun_expr :=
  | FUN; xs = ID+; RIGHTARROW; m = expr;
    { List.fold_right (fun x acc -> Fun ("", x, acc)) xs m }

let let_expr :=
  | LET; x = ID; args = arg*; ANN; t = ty; EQ; m = expr; IN; n = expr;
    { let xs, ts = List.split args in
      let m = List.fold_right (fun x acc -> Fun ("", x, acc)) xs m in
      let t = List.fold_right (fun t acc -> TArrow (t, acc)) ts t in
      Let (x, Ann (m, t), n) }

let letrec_expr :=
  | LET; REC; f = ID; arg = arg; args = arg*; ANN; t = ty; EQ; m = expr; IN; n = expr;
    { let x0, t0 = arg in
      let xs, ts = List.split args in
      let m = List.fold_right (fun x acc -> Fun ("", x, acc)) xs m in
      let m = Fun (f, x0, m) in
      let t = List.fold_right (fun t acc -> TArrow (t, acc)) ts t in
      let t = TArrow (t0, t) in
      Let (f, Ann (m, t), n) }

let ifte_expr :=
  | IF; m = expr; THEN; n1 = expr; ELSE; n2 = expr;
    { Ifte (m, n1, n2) }

let expr :=
  | ~ = fun_expr; <>
  | ~ = let_expr; <>
  | ~ = letrec_expr; <>
  | ~ = ifte_expr; <>
  | ~ = expr3; <>

let prog :=
  | ~ = expr; EOF; <>
