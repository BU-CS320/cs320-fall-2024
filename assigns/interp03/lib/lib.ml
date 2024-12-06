open Utils
include My_parser

(* Exceptions *)
exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

(* 自定义字符串比较函数 *)
let str_cmp s1 s2 =
  if s1 < s2 then -1 else if s1 > s2 then 1 else 0

(* sort_uniq: sort a list and remove duplicates *)
let sort_uniq cmp lst =
  let sorted = List.sort cmp lst in
  let rec dedup acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x::acc)
    | x :: (y :: _ as tl) ->
      if cmp x y = 0 then dedup acc tl
      else dedup (x :: acc) tl
  in
  dedup [] sorted

(* for_all2: 如果两个列表长度相同且对应元素满足 p 则返回 true *)
let rec for_all2 p l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x::xs, y::ys -> p x y && for_all2 p xs ys
  | _, _ -> false

(* Apply substitution to a type *)
let rec apply_subst subst = function
  | TUnit | TInt | TFloat | TBool as t -> t
  | TVar v -> (try List.assoc v subst with Not_found -> TVar v)
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | TList t -> TList (apply_subst subst t)
  | TOption t -> TOption (apply_subst subst t)
  | TPair (t1, t2) -> TPair (apply_subst subst t1, apply_subst subst t2)

(* Occurs check *)
let rec occurs_check v = function
  | TVar x -> x = v
  | TFun (t1, t2) | TPair (t1, t2) -> occurs_check v t1 || occurs_check v t2
  | TList t | TOption t -> occurs_check v t
  | _ -> false

(* Extend substitution *)
let extend_subst subst v ty =
  (v, ty)::(List.map (fun (x, t) -> (x, apply_subst [(v, ty)] t)) subst)

(* Free vars of a type *)
let rec free_vars_of_ty = function
  | TUnit | TInt | TFloat | TBool -> []
  | TVar v -> [v]
  | TFun (t1, t2) | TPair (t1, t2) -> free_vars_of_ty t1 @ free_vars_of_ty t2
  | TList t | TOption t -> free_vars_of_ty t

(* unify_type函数用于对约束进行求解并返回统一后的类型(不进行泛化) *)
let unify_type ty constraints =
  let rec unify_constraints subst = function
    | [] -> Some subst
    | (t1, t2) :: cs ->
      let t1 = apply_subst subst t1 in
      let t2 = apply_subst subst t2 in
      match (t1, t2) with
      | TUnit, TUnit
      | TInt, TInt
      | TFloat, TFloat
      | TBool, TBool ->
          unify_constraints subst cs
      | TVar v, t | t, TVar v ->
          if occurs_check v t then None
          else unify_constraints (extend_subst subst v t) cs
      | TFun (a1, b1), TFun (a2, b2) ->
          unify_constraints subst ((a1, a2)::(b1, b2)::cs)
      | TList t1, TList t2 ->
          unify_constraints subst ((t1, t2)::cs)
      | TOption t1, TOption t2 ->
          unify_constraints subst ((t1, t2)::cs)
      | TPair (a1, b1), TPair (a2, b2) ->
          unify_constraints subst ((a1,a2)::(b1,b2)::cs)
      | _, _ -> None
  in
  match unify_constraints [] constraints with
  | Some subst ->
    let unified_ty = apply_subst subst ty in
    Some unified_ty
  | None -> None

(* generalize函数在得到最终统一后的类型后进行泛化 *)
let generalize ctxt ty =
  let ty_fv = free_vars_of_ty ty in
  let env_list = Env.to_list ctxt in
  let env_fv =
    List.fold_left (fun acc (_, Forall (vars, t)) ->
      let vars_in_t = free_vars_of_ty t in
      let all_fv = vars_in_t @ vars in
      List.fold_left (fun s x -> VarSet.union s (VarSet.of_list [x])) acc all_fv
    ) VarSet.empty env_list
  in
  let truly_free =
    List.filter (fun x -> not (VarSet.mem x env_fv)) ty_fv
    |> sort_uniq str_cmp
  in
  Forall (truly_free, ty)

(* 实现 unify 函数匹配 lib.mli 的要求 *)
let unify ty constraints =
  match unify_type ty constraints with
  | Some unified_ty ->
    let fv = sort_uniq str_cmp (free_vars_of_ty unified_ty) in
    Some (Forall (fv, unified_ty))
  | None -> None

type infer_result = ty * constr list
let fresh_ty () = TVar (gensym ())

(* Infer types and constraints *)
let rec infer ctxt expr : infer_result =
  match expr with
  | Unit -> (TUnit, [])
  | True -> (TBool, [])
  | False -> (TBool, [])
  | Int _ -> (TInt, [])
  | Float _ -> (TFloat, [])
  | Var x -> (
      match Env.find_opt x ctxt with
      | Some (Forall (vars, ty_scheme)) ->
          let subst = List.map (fun v -> (v, fresh_ty ())) vars in
          (apply_subst subst ty_scheme, [])
      | None -> failwith ("Unbound variable " ^ x)
    )
  | Annot (e, ty) ->
    let (te, ce) = infer ctxt e in
    (ty, (te, ty)::ce)
  | ESome e ->
    let (te, ce) = infer ctxt e in
    (TOption te, ce)
  | ENone ->
    let a = fresh_ty () in
    (TOption a, [])
  | Nil ->
    let a = fresh_ty () in
    (TList a, [])
  | Assert False ->
    (* assert false : α with no constraints *)
    let a = fresh_ty () in
    (a, [])
  | Assert e ->
    let (te, ce) = infer ctxt e in
    (TUnit, (te, TBool)::ce)
  | Bop (op, e1, e2) ->
    let (t1, c1) = infer ctxt e1 in
    let (t2, c2) = infer ctxt e2 in
    begin match op with
      | Add | Sub | Mul | Div | Mod ->
          (TInt, (t1, TInt)::(t2, TInt)::c1@c2)
      | AddF | SubF | MulF | DivF | PowF ->
          (TFloat, (t1, TFloat)::(t2, TFloat)::c1@c2)
      | Lt | Lte | Gt | Gte | Eq | Neq ->
          (TBool, (t1, t2)::c1@c2)
      | And | Or ->
          (TBool, (t1, TBool)::(t2, TBool)::c1@c2)
      | Comma ->
          (TPair(t1, t2), c1@c2)
      | Cons ->
          (TList t1, (t2, TList t1)::c1@c2)
      | Concat ->
          let a = fresh_ty () in
          (TList a, (t1, TList a)::(t2, TList a)::c1@c2)
    end
  | If (e1, e2, e3) ->
    let (t1, c1) = infer ctxt e1 in
    let (t2, c2) = infer ctxt e2 in
    let (t3, c3) = infer ctxt e3 in
    (t3, (t1, TBool)::(t2, t3)::c1@c2@c3)
  | Fun (arg, annot_ty, body) ->
    let a = match annot_ty with
      | Some ty -> ty
      | None -> fresh_ty ()
    in
    let ctxt' = Env.add arg (Forall([], a)) ctxt in
    let (tb, cb) = infer ctxt' body in
    (TFun (a, tb), cb)
  | App (e1, e2) ->
    let (t1, c1) = infer ctxt e1 in
    let (t2, c2) = infer ctxt e2 in
    let a = fresh_ty () in
    (a, (t1, TFun(t2,a))::c1@c2)
  | Let {is_rec; name; value; body} ->
    if is_rec then
      (* let rec f = e1 in e2:
         Introduce α, β
         Γ,f:α->β ⊢ e1:τ1 ⊣ C1
         τ1 = α->β
         Γ,f:τ1 ⊢ e2:τ2 ⊣ C2
       *)
      let alpha = fresh_ty () in
      let beta = fresh_ty () in
      let ctxt' = Env.add name (Forall([], TFun(alpha,beta))) ctxt in
      let (tv, cv) = infer ctxt' value in
      let ctxt'' = Env.add name (Forall([], tv)) ctxt in
      let (tb, cb) = infer ctxt'' body in
      (tb, (tv, TFun(alpha,beta))::cv@cb)
    else
      (* let x = e1 in e2
         Γ ⊢ e1 : τ1 ⊣ C1
         Γ,x:τ1 ⊢ e2 : τ2 ⊣ C2
       *)
      let (tv, cv) = infer ctxt value in
      let ctxt' = Env.add name (Forall([], tv)) ctxt in
      let (tb, cb) = infer ctxt' body in
      (tb, cv@cb)
  | ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
    let (tm, cm) = infer ctxt matched in
    let a = fresh_ty () in
    let ctxt' = Env.add hd_name (Forall([], a))
                  (Env.add tl_name (Forall([], TList a)) ctxt) in
    let (tc, cc) = infer ctxt' cons_case in
    let (tn, cn) = infer ctxt nil_case in
    (tn, (tm, TList a)::(tc, tn)::cm@cc@cn)
  | OptMatch {matched; some_name; some_case; none_case} ->
    let (tm, cm) = infer ctxt matched in
    let a = fresh_ty () in
    let ctxt' = Env.add some_name (Forall([], a)) ctxt in
    let (ts, cs) = infer ctxt' some_case in
    let (tn, cn) = infer ctxt none_case in
    (tn, (tm, TOption a)::(ts, tn)::cm@cs@cn)
  | PairMatch {matched; fst_name; snd_name; case} ->
    let (tm, cm) = infer ctxt matched in
    let a = fresh_ty () in
    let b = fresh_ty () in
    let ctxt' = Env.add fst_name (Forall([], a))
                  (Env.add snd_name (Forall([], b)) ctxt) in
    let (tc, cc) = infer ctxt' case in
    (tc, (tm, TPair(a,b))::cm@cc)

let type_of ctxt expr =
  let (t, c) = infer ctxt expr in
  match unify_type t c with
  | Some unified_ty ->
    Some (generalize ctxt unified_ty)
  | None -> None

(* eq_val作为独立递归函数 *)
let rec eq_val v1 v2 =
  match (v1, v2) with
  | VUnit, VUnit -> true
  | VBool b1, VBool b2 -> b1 = b2
  | VInt n1, VInt n2 -> n1 = n2
  | VFloat f1, VFloat f2 -> f1 = f2
  | VNone, VNone -> true
  | VSome x, VSome y -> eq_val x y
  | VList l1, VList l2 ->
    List.length l1 = List.length l2 &&
    for_all2 eq_val l1 l2
  | VPair (x1,y1), VPair (x2,y2) ->
    eq_val x1 x2 && eq_val y1 y2
  | _ -> false

let compare_values op v1 v2 =
  let is_fun v = match v with VClos _ -> true | _ -> false in
  if is_fun v1 || is_fun v2 then raise CompareFunVals;
  match op with
  | Eq -> VBool (eq_val v1 v2)
  | Neq -> VBool (not (eq_val v1 v2))
  | Lt | Lte | Gt | Gte ->
    let cmp v1 v2 =
      match v1, v2 with
      | VInt n1, VInt n2 -> compare n1 n2
      | VFloat f1, VFloat f2 -> compare f1 f2
      | VBool b1, VBool b2 -> compare b1 b2
      | VUnit, VUnit -> 0
      | _ -> raise CompareFunVals
    in
    let c = cmp v1 v2 in
    (match op with
     | Lt -> VBool (c < 0)
     | Lte -> VBool (c <= 0)
     | Gt -> VBool (c > 0)
     | Gte -> VBool (c >= 0)
     | _ -> failwith "unreachable")
  | _ -> failwith "Invalid op in compare_values"

let rec eval_expr env = function
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x ->
    (match Env.find_opt x env with
     | Some v -> v
     | None -> failwith ("Runtime error: unbound variable " ^ x))
  | Nil -> VList []
  | ENone -> VNone
  | ESome e -> VSome (eval_expr env e)
  | Assert e ->
    (match eval_expr env e with
     | VBool true -> VUnit
     | VBool false -> raise AssertFail
     | _ -> failwith "Assert expects bool")
  | Bop (op, e1, e2) -> eval_bop env op e1 e2
  | If (c, t, f) ->
    (match eval_expr env c with
     | VBool true -> eval_expr env t
     | VBool false -> eval_expr env f
     | _ -> failwith "Type error in if")
  | Fun (arg, _, body) -> VClos {name = None; arg; body; env}
  | App (e1, e2) ->
    (match eval_expr env e1 with
     | VClos {name=_name; arg; body; env=clos_env} ->
       let v2 = eval_expr env e2 in
       let env' = Env.add arg v2 clos_env in
       eval_expr env' body
     | _ -> failwith "Type error in application")
  | Let {is_rec; name; value; body} ->
    if is_rec then begin
      match value with
      | Fun (arg, _, bodyf) ->
        let clos = VClos {name=Some name; arg; body=bodyf; env} in
        let env' = Env.add name clos env in
        eval_expr env' body
      | _ ->
        raise RecWithoutArg
    end else
      let v = eval_expr env value in
      let env' = Env.add name v env in
      eval_expr env' body

  | ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
    (match eval_expr env matched with
     | VList [] -> eval_expr env nil_case
     | VList (hd::tl) ->
       let env' = Env.add hd_name hd (Env.add tl_name (VList tl) env) in
       eval_expr env' cons_case
     | _ -> failwith "Type error in list match")

  | PairMatch {matched; fst_name; snd_name; case} ->
    (match eval_expr env matched with
     | VPair(v1, v2) ->
       let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
       eval_expr env' case
     | _ -> failwith "Type error in pair match")

  | OptMatch {matched; some_name; some_case; none_case} ->
    (match eval_expr env matched with
     | VSome v ->
       let env' = Env.add some_name v env in
       eval_expr env' some_case
     | VNone -> eval_expr env none_case
     | _ -> failwith "Type error in option match")

  | Annot (e, _) -> eval_expr env e

and eval_bop env op e1 e2 =
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  match op, v1, v2 with
  (* Integer arithmetic *)
  | Add, VInt n1, VInt n2 -> VInt (n1+n2)
  | Sub, VInt n1, VInt n2 -> VInt (n1-n2)
  | Mul, VInt n1, VInt n2 -> VInt (n1*n2)
  | Div, VInt n1, VInt n2 ->
    if n2 = 0 then raise DivByZero else VInt (n1/n2)
  | Mod, VInt n1, VInt n2 ->
    if n2 = 0 then raise DivByZero else VInt (n1 mod n2)

  (* Floating-point arithmetic *)
  | AddF, VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
  | SubF, VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
  | MulF, VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
  | DivF, VFloat f1, VFloat f2 ->
    if f2 = 0.0 then raise DivByZero else VFloat (f1 /. f2)
  | PowF, VFloat f1, VFloat f2 -> VFloat (f1 ** f2)

  (* Boolean logic *)
  | And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Or, VBool b1, VBool b2 -> VBool (b1 || b2)

  (* List operations *)
  | Cons, v, VList l -> VList (v::l)
  | Concat, VList l1, VList l2 -> VList (l1 @ l2)

  (* Pair construction *)
  | Comma, v1, v2 -> VPair(v1, v2)

  (* Comparisons *)
  | Lt, _, _
  | Lte, _, _
  | Gt, _, _
  | Gte, _, _
  | Eq, _, _
  | Neq, _, _ ->
     compare_values op v1 v2

  | _ -> failwith "Invalid operand types for operator"

let type_check =
  let rec go ctxt = function
    | [] -> Some (Forall ([], TUnit))
    | {is_rec;name;value} :: ls ->
      match type_of ctxt (Let {is_rec;name;value;body=Var name}) with
      | Some ty ->
        (match ls with
         | [] -> Some ty
         | _ ->
           let ctxt = Env.add name ty ctxt in
           go ctxt ls)
      | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] ->
       Let {is_rec;name;value;body=Var name}
    | {is_rec;name;value}::ls ->
       Let {is_rec;name;value;body=nest ls}
  in
  eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
