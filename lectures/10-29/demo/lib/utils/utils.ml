type expr =
  | Num of int
  | True
  | False
  | Add of expr * expr
  | Eq of expr * expr

type prog = expr

type value =
  | VNum of int
  | VBool of bool
