type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

(* Surface-level expressions (produced by the parser) *)
type sfexpr =
  | SNum of int
  | SVar of string
  | SUnit | STrue | SFalse
  | SApp of sfexpr * sfexpr
  | SBop of bop * sfexpr * sfexpr
  | SIf of sfexpr * sfexpr * sfexpr
  | SLet of { is_rec: bool; name: string; ty: ty; value: sfexpr; body: sfexpr }
  | SFun of string * ty * sfexpr
  | SAssert of sfexpr
  | SToplets of sftoplet list

and sftoplet =
  | SToplet of { is_rec: bool; name: string; args: (string * ty) list; ty: ty; value: sfexpr }

type ty =
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy of ty * ty
