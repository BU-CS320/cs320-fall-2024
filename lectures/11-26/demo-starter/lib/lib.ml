open Stdlib320
open Utils

let parse s =
  try Some (Par.prog Lex.read (Lexing.from_string s))
  with _ -> None

let ty_subst t x =
  let rec go = function
    | TInt -> TInt
    | TBool -> TBool
    | TVar y -> if x = y then t else TVar y
    | TFun (t1, t2) -> TFun (go t1, go t2)
  in go

let ty_subst_c t x (t1, t2) = (ty_subst t x t1, ty_subst t x t2)
let ty_subst_cs t x = List.map (ty_subst_c t x)

let rec fvs = function
  | TInt -> VarSet.empty
  | TBool -> VarSet.empty
  | TVar x -> VarSet.of_list [x]
  | TFun (t1, t2) -> VarSet.union (fvs t1) (fvs t2)

let unify _ = assert false

let type_of _ = assert false

let eval =
  let rec eval env =
    let rec go = function
      | Var x -> Env.find x env
      | Num n -> VNum n
      | Fun (arg, body) -> VClos {name=None;arg;body;env}
      | App (e1, e2) -> (
        match go e1 with
        | VClos info ->
          let env =
            match info.name with
            | None -> env
            | Some name -> Env.add name (VClos info) info.env
          in
          let env = Env.add info.arg (go e2) env in
          eval env info.body
        | _ -> failwith "impossible"
      )
      | Eq (e1, e2) -> (
        match go e1, go e2 with
        | VNum m, VNum n -> VBool (m = n)
        | _ -> failwith "impossible"
      )
      | Add (e1, e2) -> (
        match go e1, go e2 with
        | VNum m, VNum n -> VNum (m + n)
        | _ -> failwith "impossible"
      )
      | If (e1, e2, e3) -> (
        match go e1 with
        | VBool true -> go e2
        | VBool false -> go e3
        | _ -> failwith "impossible"
      )
      | Let (x, e1, e2) ->
        let env = Env.add x (go e1) env in
        eval env e2
      | LetRec (f, arg, body, e) ->
        let clos = VClos {name=Some f;arg;body;env} in
        let env = Env.add f clos env in
        eval env e
    in go
  in eval Env.empty

let string_of_value = function
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClos _ -> "<fun>"

let string_of_ty_scheme (fvs, t) =
  let letter i = String.init 1 (fun _ -> (char_of_int (i + 97))) in
  let rec clean i fvs t =
    match fvs with
    | [] -> t
    | x :: xs -> clean (i + 1) xs (ty_subst (TVar ("\'" ^ letter i)) x t)
  in
  let rec go = function
    | TInt -> "int"
    | TBool -> "Bool"
    | TVar x -> x
    | TFun (TFun (t1, t2), t) -> "(" ^ go (TFun (t1, t2)) ^ ") -> " ^ go t
    | TFun (t1, t2) -> go t1 ^ " -> " ^ go t2
  in go (clean 0 fvs t)

let interp prog =
  match parse prog with
  | Some expr -> (
    match type_of expr with
    | Some ty ->
      print_endline
      ( "\n"
      ^ "- : "
      ^ string_of_ty_scheme ty
      ^ " = "
      ^ string_of_value (eval expr)
      ^ "\n"
      )
    | None -> failwith "Type error"
  )
  | None -> failwith "Parse error"

