%{
open Utils
%}

%token <int> NUM
%token EOF

%start <Utils.prog> prog

%%

prog:
  | EOF { Num 0 }
