(* Helper function to sum the last n elements of a list *)
let sum_last_n lst n =
  let rec aux acc count = function
    | [] -> acc
    | x :: xs -> if count < n then aux (acc + x) (count + 1) xs
                 else aux acc count xs
  in aux 0 0 (List.rev lst)

(* Generalized Fibonacci function *)
let gen_fib l k =
  let len_l = List.length l in
  if k < len_l then List.nth l k
  else
    let rec extend_list current =
      if List.length current <= k then
        let next_val = sum_last_n current len_l in
        extend_list (current @ [next_val])
      else current
    in
    let final_list = extend_list l in
    List.nth final_list k
