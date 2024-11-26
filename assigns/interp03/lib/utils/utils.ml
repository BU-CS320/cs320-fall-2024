type ident = string

type ty =
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TVar of ident
  | TList of ty
  | TOption of ty
  | TPair of ty * ty
  | TFun of ty * ty

type ty_scheme =
  | Forall of ident list * ty

type bop =
  | Add | Sub | Mul | Div | Mod
  | AddF | SubF | MulF | DivF | PowF
  | Cons | Concat
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or
  | Comma

type expr =
  | Unit | True | False | Nil | ENone
  | Int of int | Float of float
  | Var of ident
  | Assert of expr
  | ESome of expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | ListMatch of
    { matched : expr
    ; hd_name : ident
    ; tl_name : ident
    ; cons_case : expr
    ; nil_case : expr
    }
  | OptMatch of
    { matched : expr
    ; some_name : ident
    ; some_case : expr
    ; none_case : expr
    }
  | PairMatch of
    { matched : expr
    ; fst_name : ident
    ; snd_name : ident
    ; case : expr
    }
  | Fun of ident * ty option * expr
  | App of expr * expr
  | Annot of expr * ty
  | Let of
    { is_rec : bool
    ; name : ident
    ; value : expr
    ; body : expr
    }

type toplet =
  { is_rec : bool
  ; name : ident
  ; value : expr
  }

type prog = toplet list

type constr = ty * ty

type stc_env = ty_scheme env
type var_set = unit env

module VarSet = struct
  let mem = Env.mem
  let empty = Env.empty
  let union = Env.union (fun _ () () -> Some ())
  let of_list l = Env.of_list (List.map (fun x -> x, ()) l)
  let to_list s = List.map (fun (x, _) -> x) (Env.to_list s)
end

type dyn_env = value env

and value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VList of value list
  | VPair of value * value
  | VNone
  | VSome of value
  | VClos of
    { name : string option
    ; arg : string
    ; body : expr
    ; env : dyn_env
    }

type error =
  | ParseError
  | TypeError

let err_msg = function
  | ParseError -> "parse error"
  | TypeError -> "type error"
