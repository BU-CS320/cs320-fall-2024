
let last_function_standing fs start pred =
  let step fxs =
    fxs
    |> List.map (fun (f, x) -> (f, f x))
    |> List.filter (fun (_, x) -> not (pred x))
  in
  let rec go = function
    | [] -> None
    | [f, _] -> Some f
    | fxs -> go (step fxs)
  in
  let init = List.map (fun f -> (f, start)) fs in
  go init
