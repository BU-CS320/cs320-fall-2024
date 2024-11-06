type expr =
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr

type prog = (string * expr) list

type value =
  | VFun of string * expr
