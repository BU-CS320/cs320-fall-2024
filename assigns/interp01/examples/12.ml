let a = fun _ ->
  let _ = (fun x -> x x) (fun x -> x x) in
  true
in false && a
