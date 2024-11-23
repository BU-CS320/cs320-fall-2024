
open Lab


(* Program Pipeline

syntax.ml : abstract syntax of expressions
lex.mll : lexer consumes strings to produce tokens
par.mly : parser consumes tokens to produce expression
eval.ml : evaluates the expressions to values *)

let () =
  let fname = Sys.argv.(1) in
  let c = open_in fname in
  let m = Parser.parse c in
  let t = Tcheck.infer [] m in
  (* let v = Eval.eval [] m in *)
  Format.printf "input: %a@." Syntax.pp_expr m;
  Format.printf "type: %a@." Syntax.pp_ty t
  (* Format.printf "value: %a@." Syntax.pp_value v *)

