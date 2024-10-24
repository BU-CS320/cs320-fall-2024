let rec type_of (e: Utils.expr) : Utils.ty option =
  match e with
  | Utils.Num _ -> Some Utils.TInt
  | Utils.Add(e1, e2) ->
    (match (type_of e1, type_of e2) with
     | (Some Utils.TInt, Some Utils.TInt) -> Some Utils.TInt
     | _ -> None)
  | Utils.Lt(e1, e2) ->
    (match (type_of e1, type_of e2) with
     | (Some Utils.TInt, Some Utils.TInt) -> Some Utils.TBool
     | _ -> None)
  | Utils.Ite(e1, e2, e3) ->
    (match (type_of e1, type_of e2, type_of e3) with
     | (Some Utils.TBool, Some t2, Some t3) when t2 = t3 -> Some t2
     | _ -> None)
