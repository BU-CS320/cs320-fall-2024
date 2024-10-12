

(*
Formal Syntax:
<expr> ::=
if <expr> then <expr> else <expr> |
let <var> = <expr> in <expr> |
true |
false |
<var>   
*)
(*
type expr =
  | If of expr * expr * expr   (* if x then y else z -> If(Var("x"), Var("y"), Var("z")) *)
  | Let of string * expr * expr (* let x = true in x  -> Let("x", True, Var("x")) *)
  | True
  | False
  | Var of string

(*






var has size n    e1 has size n1     e2 has size n2
------------------------------------------------------LET
let var = e1 in e2   has size 3 + n + 1 + n1 + 2 + n2
   



e has size n     e1 has size n1     e2 has size n2
--------------------------------------------------------IF
if e then e1 else e2   has size 2 + n + 4 + n1 + 4 + n2

---------------TT
true has size 4

----------------FF
false has size 5

len(var) = s
--------------VAR
var has size s
*)

let rec size expr =
  match expr with
  | Let(var, e1, e2) ->
    let n = size (Var var) in
    let n1 = size e1 in
    let n2 = size e2 in
    3 + n + 1 + n1 + 2 + n2
  | If(e, e1, e2) ->
    let n = size e in
    let n1 = size e1 in
    let n2 = size e2 in
    2 + n + 4 + n1 + 4 + n2
  | True -> 4
  | False -> 5
  | Var(var) -> String.length var


----------------TV
true \||/ true

------------------FV
false \||/ false


e \||/ true      e1 \||/ v1
-----------------------------IF-THEN
if e then e1 else e2 \||/ v1



e \||/ false      e2 \||/ v2
-----------------------------IF-ELSE
if e then e1 else e2 \||/ v2



e1 \||/ v1           [v1/x] e2 \||/ v2
----------------------------------------LET
let x = e1 in e2 \||/ v2
*)
(*
type value =
  | TrueVal
  | FalseVal

exception NotPossible;;

let rec value expr =
  match expr with
  | Let(x, e1, e2) ->
      let v1 = value e1 in
      let v2 = value (substitute v1 x e2) in
      v2
  | If(e, e1, e2) ->
      (let v = value e in
      match v with
      | TrueVal -> value e1
      | FalseVal -> value e2)
  | True -> TrueVal
  | False -> FalseVal
  | Var(_var) -> raise NotPossible

val substitute : value -> string -> expr -> expr

type validity =
  | Valid
  | Invalid

val parser : string -> expr

val typecheck : context -> expr -> tp -> validity

val type_infer : context -> expr -> tp

val pretty_print : context -> expr -> string

val value : expr -> value

{x : int, y : bool, z : float}

type context = (string, tp) list
*)
let () = print_endline "Hello, World!"

