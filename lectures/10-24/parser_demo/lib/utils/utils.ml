type bop =
  | Add
  | Sub
  | Mul
  | Div


type expr =
  | Var of string
  | Num of int
  | Let of string * expr * expr
  | Bop of bop * expr * expr

type prog = expr
