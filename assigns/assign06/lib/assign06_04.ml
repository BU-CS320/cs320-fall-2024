let rec eval (e: Utils.expr) : Utils.value =
  match e with
  | Utils.Num n -> Utils.VNum n
  | Utils.Add(e1, e2) ->
    (match (eval e1, eval e2) with
     | (Utils.VNum v1, Utils.VNum v2) -> Utils.VNum (v1 + v2)
     | _ -> failwith "Type error")
  | Utils.Lt(e1, e2) ->
    (match (eval e1, eval e2) with
     | (Utils.VNum v1, Utils.VNum v2) -> Utils.VBool (v1 < v2)
     | _ -> failwith "Type error")
  | Utils.Ite(e1, e2, e3) ->
    (match eval e1 with
     | Utils.VBool true -> eval e2
     | Utils.VBool false -> eval e3
     | _ -> failwith "Type error")
