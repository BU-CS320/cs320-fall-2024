
open Syntax
open Format

type ctx = (string * ty) list
let rec pp_ctx fmt (ctx : ctx) =
  fprintf fmt "[";
  List.iter (fun (x, v) -> fprintf fmt "(%s, %a); " x pp_ty v) ctx;
  fprintf fmt "]"

let ty_equal t1 t2 =
  failwith "undefined"

let rec infer (ctx : ctx) (e : expr) : ty =
  failwith "undefined"

and check ctx m t : unit =
  failwith "undefined"