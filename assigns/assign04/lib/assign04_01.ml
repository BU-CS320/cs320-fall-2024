(* assign04_01.ml *)

let max_steps = 10000 (* Set an upper bound on recursion depth *)

(* Tail-recursive function to compute lifespan of a function *)
let rec lifespan f s p steps =
  if steps >= max_steps || p s then steps
  else lifespan f (f s) p (steps + 1)

let last_function_standing funcs start pred =
  let rec aux funcs best_func best_lifespan =
    match funcs with
    | [] -> best_func
    | f :: fs ->
      let current_lifespan = lifespan f start pred 0 in
      if current_lifespan > best_lifespan then
        aux fs (Some f) current_lifespan
      else if current_lifespan = best_lifespan then
        aux fs None best_lifespan
      else
        aux fs best_func best_lifespan
  in
  aux funcs None (-1)
