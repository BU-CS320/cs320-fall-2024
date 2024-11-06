
open Syntax
include Parser

let rec subst x m n =
  failwith "unimplemented"

(* Think of the return `v` of `eval m` as `m ⇓ v` *)
let rec eval m =
  failwith "unimplemented"

let interp fname =
  let c = open_in fname in
  let m = Parser.parse c in
  let v = eval m in
  v
