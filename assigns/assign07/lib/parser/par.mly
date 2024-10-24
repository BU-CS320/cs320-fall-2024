%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token EOF

%start <Utils.prog> prog

%%

prog:
  | EOF { Num 0 }
