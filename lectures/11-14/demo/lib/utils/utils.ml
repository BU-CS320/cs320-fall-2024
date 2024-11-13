type ty =
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy of ty * ty

type expr =
  | True
  | False
  | Var of string
  | Num of int
  | Fun of string * ty * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of string * ty * expr * expr
  | LetRec of string * string * ty * ty * expr * expr

type prog = expr

type value =
  | VBool of bool
  | VNum of int
  | VClos of string * expr * value env * string option
