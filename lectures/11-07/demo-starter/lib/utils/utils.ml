type expr =
  | True
  | False
  | Var of string
  | Num of int
  | Fun of string * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of string * expr * expr

type prog = expr

type value =
  | VBool of bool
  | VNum of int
  | VFun of string * expr
