
type unary_op =
  | Neg
  | Not

type binary_op =
  (* integer *)
  | Add
  | Sub
  | Mul
  | Div
  (* boolean *)
  | And
  | Or
  (* compare *)
  | Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Neq

type expr =
  | Int of int
  | Bool of bool
  | UnaryOp of unary_op * expr
  | BinaryOp of binary_op * expr * expr
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Ifte of expr * expr * expr
