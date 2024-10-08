
let rec group l =
  match l with
  | [] -> Some []
  | [n] ->
     if n <> 0
     then Some [[n]]
     else None
  | m :: 0 :: n :: l ->
     if m * n < 0
     then
       match group (n :: l) with
       | Some ls -> Some ([m] :: ls)
       | None -> None
     else None
  | m :: n :: l ->
     if m * n > 0
     then
       match group (n :: l) with
       | Some (xs :: ls) -> Some ((m :: xs) :: ls)
       | _ -> None
     else None
