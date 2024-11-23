type ty =
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy of ty * ty

type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

type sfexpr =
  | SUnit
  | STrue
  | SFalse
  | SNum of int
  | SVar of string
  | SFun of { arg : string * ty; args : (string * ty) list; body : sfexpr }
  | SApp of sfexpr * sfexpr
  | SLet of { is_rec : bool; name : string; args : (string * ty) list; ty : ty; value : sfexpr; body : sfexpr }
  | SIf of sfexpr * sfexpr * sfexpr
  | SBop of bop * sfexpr * sfexpr

type toplet =
  { is_rec : bool
  ; name : string
  ; args : (string * ty) list
  ; ty : ty
  ; value : sfexpr
  }

type prog = toplet list
