%{
open Utils
%}

%token EOF

%start <Utils.prog> prog

%%

prog:
  | EOF { Num 0 }
