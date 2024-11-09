include Utils  (* Re-export everything from Utils to make it accessible *)

open My_parser  (* Import the parse function correctly *)

(* Re-export the parse function to make it accessible as Lib.parse *)
let parse = parse

(* Substitute value `v` for variable `x` in expression `e`, capture-avoiding *)
let rec subst v x e =
  match e with
  | Var y when y = x -> v  (* Replace the variable *)
  | Var y -> Var y  (* Leave other variables unchanged *)
  | Num n -> Num n
  | Unit -> Unit
  | True -> True
  | False -> False
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) when y <> x -> Let (y, subst v x e1, subst v x e2)
  | Fun (y, e_body) when y <> x -> Fun (y, subst v x e_body)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | _ -> e  (* Handle other cases appropriately *)

(* Evaluation function *)
let rec eval expr =
  match expr with
  | Num n -> Ok (Num n)
  | True -> Ok True
  | False -> Ok False
  | Unit -> Ok Unit

  (* Arithmetic operations *)
  | Bop (Add, Num n1, Num n2) -> Ok (Num (n1 + n2))
  | Bop (Sub, Num n1, Num n2) -> Ok (Num (n1 - n2))
  | Bop (Mul, Num n1, Num n2) -> Ok (Num (n1 * n2))
  | Bop (Div, _, Num 0) -> Error DivByZero
  | Bop (Div, Num n1, Num n2) -> Ok (Num (n1 / n2))
  | Bop (Mod, _, Num 0) -> Error DivByZero
  | Bop (Mod, Num n1, Num n2) -> Ok (Num (n1 mod n2))

  (* Boolean operations *)
  | If (cond, e_then, e_else) ->
      (match eval cond with
       | Ok True -> eval e_then
       | Ok False -> eval e_else
       | _ -> Error InvalidIfCond)

  (* Let bindings *)
  | Let (x, e1, e2) ->
      (match eval e1 with
       | Ok v -> eval (subst v x e2)
       | Error _ as err -> err)

  (* Function application *)
  | App (Fun (x, e_body), e_arg) ->
      (match eval e_arg with
       | Ok v -> eval (subst v x e_body)
       | Error _ as err -> err)
  | App (_, _) -> Error InvalidApp

  (* Error cases for invalid operators and unknown variables *)
  | Bop (op, _, _) -> Error (InvalidArgs op)
  | Var x -> Error (UnknownVar x)
  | Fun (x, e_body) -> Ok (Fun (x, e_body))  (* Return function itself as a value if needed *)



(* Interpreter function *)
let interp s =
  match parse s with
  | Some prog -> eval prog
  | None -> Error ParseFail
