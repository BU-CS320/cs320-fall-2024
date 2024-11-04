%{
(*
<prog>  ::= <expr>
<expr>  ::= fun <var> -> expr
         | <expr2> <expr2>*
<expr2> ::= <var> | ( <expr> )
*)

open Utils
%}

%token EOF

%start <Utils.prog> prog

%%

prog:
  | EOF { Var "x" }
