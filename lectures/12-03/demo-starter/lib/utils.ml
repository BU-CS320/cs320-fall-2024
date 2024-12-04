open Stdlib320

type expr =
  | Let of string * expr * expr
  | LetRec of string * string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Add of expr * expr
  | Eq of expr * expr
  | Num of int
  | Var of string

type ty =
  | TInt
  | TBool
  | TVar of string
  | TFun of ty * ty

type prog = expr

type value =
  | VNum of int
  | VBool of bool
  | VClos of
    { name: string option
    ; arg: string
    ; body: expr
    ; env: value env
    }

type var_set = unit env

module VarSet = struct
  let mem = Env.mem
  let empty = Env.empty
  let union = Env.union (fun _ () () -> Some ())
  let of_list l = Env.of_list (List.map (fun x -> x, ()) l)
  let to_list s = List.map (fun (x, _) -> x) (Env.to_list s)
end


