open Utils
include My_parser

(* Exception Declarations *)
exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

(* Helper to Get Free Variables *)
let rec free_vars ty =
  match ty with
  | TVar x -> [x]
  | TFun (t1, t2) | TPair (t1, t2) -> free_vars t1 @ free_vars t2
  | TList t | TOption t -> free_vars t
  | _ -> []

(* Substitute a Variable with Another Type in a Type Expression *)
let rec substitute var subst ty =
  match ty with
  | TVar x -> if x = var then subst else ty
  | TFun (t1, t2) -> TFun (substitute var subst t1, substitute var subst t2)
  | TPair (t1, t2) -> TPair (substitute var subst t1, substitute var subst t2)
  | TList t -> TList (substitute var subst t)
  | TOption t -> TOption (substitute var subst t)
  | _ -> ty

(* Type Unification Function *)
let unify (ty : Utils.ty) (constraints : Utils.constr list) : Utils.ty_scheme option =
  let rec unify_one (t1, t2) subs =
    match t1, t2 with
    | TInt, TInt | TBool, TBool | TUnit, TUnit -> subs
    | TVar x, t | t, TVar x ->
        if t = TVar x then subs
        else if List.mem x (free_vars t) then failwith "Occurs check failed"
        else (x, t) :: subs
    | TFun (t1a, t1b), TFun (t2a, t2b) ->
        unify_one (t1a, t2a) subs |> unify_one (t1b, t2b)
    | TPair (t1a, t1b), TPair (t2a, t2b) ->
        unify_one (t1a, t2a) subs |> unify_one (t1b, t2b)
    | TList t1, TList t2 | TOption t1, TOption t2 ->
        unify_one (t1, t2) subs
    | _ -> failwith "Unification failed: incompatible types"
  in
  try
    let subs = List.fold_left (fun subs constr -> unify_one constr subs) [] constraints in
    let ty_subst = List.fold_left (fun t (x, s) -> substitute x s t) ty subs in
    Some (Forall (free_vars ty_subst, ty_subst))
  with _ -> None

(* Type Inference Function *)
let rec type_of env expr =
  match expr with
  | Unit -> Some (Forall ([], TUnit))
  | True | False -> Some (Forall ([], TBool))
  | Int _ -> Some (Forall ([], TInt))
  | Float _ -> Some (Forall ([], TFloat))
  | Var x -> Env.find_opt x env
  | Fun (arg, ty_opt, body) ->
      let arg_ty = match ty_opt with Some ty -> ty | None -> TVar (gensym ()) in
      let new_env = Env.add arg (Forall ([], arg_ty)) env in
      (match type_of new_env body with
      | Some (Forall (_, body_ty)) -> Some (Forall ([], TFun (arg_ty, body_ty)))
      | None -> None)
  | Let { is_rec; name; value; body } ->
      let value_ty =
        if is_rec then
          let arg_ty = TVar (gensym ()) in
          let func_ty = TFun (arg_ty, arg_ty) in
          Some (Forall ([], func_ty))
        else type_of env value
      in
      (match value_ty with
      | Some ty ->
          let new_env = Env.add name ty env in
          type_of new_env body
      | None -> None)
  | If (e1, e2, e3) ->
      (match type_of env e1, type_of env e2, type_of env e3 with
      | Some (Forall (_, TBool)), Some t2, Some t3 when t2 = t3 -> Some t2
      | _ -> None)
  | _ -> None

(* Binary Operator Evaluation Function *)
let eval_bop op v1 v2 =
  match op, v1, v2 with
  | Add, VInt x, VInt y -> VInt (x + y)
  | Sub, VInt x, VInt y -> VInt (x - y)
  | Mul, VInt x, VInt y -> VInt (x * y)
  | Div, VInt x, VInt y -> if y = 0 then raise DivByZero else VInt (x / y)
  | Mod, VInt x, VInt y -> if y = 0 then raise DivByZero else VInt (x mod y)
  | AddF, VFloat x, VFloat y -> VFloat (x +. y)
  | SubF, VFloat x, VFloat y -> VFloat (x -. y)
  | MulF, VFloat x, VFloat y -> VFloat (x *. y)
  | DivF, VFloat x, VFloat y -> VFloat (x /. y)
  | PowF, VFloat x, VFloat y -> VFloat (x ** y)
  | Lt, VInt x, VInt y -> VBool (x < y)
  | Lte, VInt x, VInt y -> VBool (x <= y)
  | Gt, VInt x, VInt y -> VBool (x > y)
  | Gte, VInt x, VInt y -> VBool (x >= y)
  | Eq, VInt x, VInt y -> VBool (x = y)
  | Neq, VInt x, VInt y -> VBool (x <> y)
  | And, VBool x, VBool y -> VBool (x && y)
  | Or, VBool x, VBool y -> VBool (x || y)
  | Cons, v, VList vs -> VList (v :: vs)
  | Concat, VList l1, VList l2 -> VList (l1 @ l2)
  | _, _, _ -> failwith "Invalid binary operation or operand types"

(* Expression Evaluation Function *)
let rec eval_expr env expr =
  match expr with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x -> (match Env.find_opt x env with Some v -> v | None -> failwith ("Unbound variable: " ^ x))
  | If (e1, e2, e3) ->
      (match eval_expr env e1 with
      | VBool true -> eval_expr env e2
      | VBool false -> eval_expr env e3
      | _ -> failwith "Condition of if must be a boolean")
  | Bop (op, e1, e2) -> eval_bop op (eval_expr env e1) (eval_expr env e2)
  | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
  | App (e1, e2) ->
      (match eval_expr env e1 with
      | VClos { name = _; arg; body; env = clos_env } ->
          let arg_val = eval_expr env e2 in
          eval_expr (Env.add arg arg_val clos_env) body
      | _ -> failwith "Attempt to apply a non-function")
  | Let { is_rec; name; value; body } ->
      let rec_env = if is_rec then Env.add name VNone env else env in
      let value_val = eval_expr rec_env value in
      let final_env = Env.add name value_val env in
      eval_expr final_env body
  | _ -> failwith "Unimplemented expression"

(* Type Check Function *)
let type_check =
  let rec go ctxt = function
    | [] -> Some (Forall ([], TUnit))
    | { is_rec; name; value } :: ls -> (
        match type_of ctxt (Let { is_rec; name; value; body = Var name }) with
        | Some ty ->
            let ctxt = Env.add name ty ctxt in
            go ctxt ls
        | None -> None)
  in
  go Env.empty

(* Program Evaluation Function *)
let eval p =
  let rec nest = function
    | [] -> Unit
    | [{ is_rec; name; value }] -> Let { is_rec; name; value; body = Var name }
    | { is_rec; name; value } :: ls -> Let { is_rec; name; value; body = nest ls }
  in
  eval_expr Env.empty (nest p)

(* Interpreter Entry Point *)
let interp input =
  match parse input with
  | Some prog -> (
      match type_check prog with
      | Some ty -> Ok (eval prog, ty)
      | None -> Error TypeError)
  | None -> Error ParseError


