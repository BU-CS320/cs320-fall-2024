include Utils
open My_parser

let parse s =
  try Some (Par.prog Lex.read (Lexing.from_string s))
  with _ -> None

let rec desugar_expr = function
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SFun { arg = (x, ty); args; body } ->
      List.fold_right (fun (x, ty) acc -> Fun (x, ty, acc)) ((x, ty) :: args) (desugar_expr body)
  | SApp (e1, e2) -> App (desugar_expr e1, desugar_expr e2)
  | SLet { is_rec; name; args; ty; value; body } ->
      Let { is_rec; name; ty; value = desugar_expr value; body = desugar_expr body }
  | SIf (cond, e_then, e_else) -> If (desugar_expr cond, desugar_expr e_then, desugar_expr e_else)
  | SBop (op, e1, e2) -> Bop (op, desugar_expr e1, desugar_expr e2)

let desugar prog =
  List.fold_right
    (fun toplet acc ->
       Let
         { is_rec = toplet.is_rec
         ; name = toplet.name
         ; ty = toplet.ty
         ; value = desugar_expr toplet.value
         ; body = acc
         })
    prog
    Unit

let rec type_of ctx expr = (* Existing logic here *)

let rec eval expr = (* Existing logic here *)

let interp s =
  match parse s with
  | Some prog -> (
      let expr = desugar prog in
      match type_of [] expr with
      | Ok _ -> eval expr
      | Error e -> Error e
    )
  | None -> Error ParseFail
