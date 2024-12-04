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

let unify =
  let rec go = function
    | [] -> None
    | [TVar "_out", t] -> Some t (* optimization to not build a full solution *)
    | (t1, t2) :: cs when t1 = t2 -> go cs
    | (TFun (t1, t2), TFun (t1', t2')) :: cs ->
      go ((t1, t1') :: (t2, t2') :: cs)
    | (TVar x, t) :: cs ->
      if VarSet.mem x (fvs t)
      then None
      else go (ty_subst_cs t x cs)
    | (t, TVar x) :: cs -> go ((TVar x, t) :: cs)
    | _ -> None
  in go

let rec instantiate (fvs, t) =
  match fvs with
  | [] -> t
  | x :: xs ->
    let fresh = TVar (gensym ()) in
    ty_subst fresh x (instantiate (xs, t))

let rec type_of ctxt =
  let rec go = function
  | Num _ -> TInt, []
  | Var x ->
    let ty_scheme = Env.find x ctxt in
    instantiate ty_scheme, []
  | Add (e1, e2) ->
    let t1, c1 = go e1 in
    let t2, c2 = go e2 in
    ( TInt
    , (t1, TInt) :: (t2, TInt) :: c1 @ c2
    )
  | Eq (e1, e2) ->
    let t1, c1 = go e1 in
    let t2, c2 = go e2 in
    ( TBool
    , (t1, TInt) :: (t2, TInt) :: c1 @ c2
    )
  | Fun (x, e) ->
    let fresh = TVar (gensym ()) in
    let ctxt = Env.add x ([], fresh) ctxt in
    let t, c = type_of ctxt e in
    ( TFun (fresh, t)
    , c
    )
  | App (e1, e2) ->
    let t1, c1 = go e1 in
    let t2, c2 = go e2 in
    let fresh = TVar (gensym ()) in
    ( fresh
    , (t1, TFun (t2, fresh)) :: c1 @ c2
    )
  | If (e1, e2, e3) ->
    let t1, c1 = go e1 in
    let t2, c2 = go e2 in
    let t3, c3 = go e3 in
    ( t3
    , (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3
    )
  | Let (x, e1, e2) ->
    let t1, c1 = go e1 in
    let t2, c2 =
      let ctxt = Env.add x ([], t1) ctxt in
      type_of ctxt e2
    in (t2, c1 @ c2)
  | LetRec (f, x, e1, e2) ->
    let fresh1 = TVar (gensym ()) in
    let fresh2 = TVar (gensym ()) in
    let ctxt = Env.add f ([], TFun (fresh1, fresh2)) ctxt in
    let t1, c1 =
      let ctxt = Env.add x ([], fresh1) ctxt in
      type_of ctxt e1
    in
    let t2, c2 = type_of ctxt e2 in
    ( t2
    , (fresh2, t1) :: c1 @ c2
    )
  in go

let type_of e =
  let t, c = type_of Env.empty e in
  let t = unify (c @ [TVar "_out", t]) in
  match t with
  | Some t -> Some (VarSet.to_list (fvs t), t)
  | None -> None

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
    | TBool -> "bool"
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

