type expr =
  | Var of string
  | Fun of string * expr
  | App of expr * expr

type prog = expr

type value =
  | VFun of string * expr
