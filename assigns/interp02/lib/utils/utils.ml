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

type sftoplet =
  | SToplet of { is_rec: bool; name: string; args: (string * ty) list; ty: ty; value: sfexpr }

(* Core language expressions (used after desugaring) *)
type expr =
  | Num of int
  | Var of string
  | Unit | True | False
  | App of expr * expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Let of { is_rec: bool; name: string; ty: ty; value: expr; body: expr }
  | Fun of string * ty * expr
  | Assert of expr

type ty =
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy of ty * ty

type value =
  | VNum of int
  | VBool of bool
  | VUnit
  | VFun of string * expr

type error =
  | DivByZero
  | InvalidIfCond
  | InvalidArgs of bop
  | InvalidApp
  | UnknownVar of string
  | ParseFail

let string_of_value = function
  | VNum n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VUnit -> "()"
  | VFun (_, _) -> "<fun>"

let string_of_bop = function
  | Add -> "(+)"
  | Sub -> "(-)"
  | Mul -> "(*)"
  | Div -> "(/)"
  | Mod -> "(mod)"
  | Lt -> "(<)"
  | Lte -> "(<=)"
  | Gt -> "(>)"
  | Gte -> "(>=)"
  | Eq -> "(=)"
  | Neq -> "(<>)"
  | And -> "(&&)"
  | Or -> "(||)"

let err_msg = function
  | DivByZero -> "division by zero"
  | InvalidIfCond -> "non-Boolean value given as condition"
  | InvalidArgs op -> "invalid arguments given to " ^ string_of_bop op
  | InvalidApp -> "non-function value used in function application"
  | UnknownVar x -> "unknown variable '" ^ x ^ "'"
  | ParseFail -> "syntax error"
