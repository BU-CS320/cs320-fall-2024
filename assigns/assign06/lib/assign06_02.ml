open Utils  (* Keep this if you need Utils *)

  let parse (tokens: Utils.tok list) : expr option =  (* Use Utils.tok *)
    let rec helper stack = function
      | [] -> (match stack with
               | [e] -> Some e
               | _ -> None)
      | Utils.TNum n :: ts -> helper (Num n :: stack) ts
      | Utils.TAdd :: ts -> 
        (match stack with
         | e1 :: e2 :: rest -> helper (Add(e2, e1) :: rest) ts
         | _ -> None)
      | Utils.TLt :: ts -> 
        (match stack with
         | e1 :: e2 :: rest -> helper (Lt(e2, e1) :: rest) ts
         | _ -> None)
      | Utils.TIte :: ts -> 
        (match stack with
         | e1 :: e2 :: e3 :: rest -> helper (Ite(e3, e2, e1) :: rest) ts
         | _ -> None)
    in
    helper [] tokens
  
