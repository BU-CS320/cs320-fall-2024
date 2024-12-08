(* Helper function to validate the list *)
let rec is_valid lst prev =
  match lst with
  | [] -> true  (* Empty list is valid *)
  | 0 :: t when prev <> 0 -> (
      match t with
      | [] -> false  (* Zero can't be at the end *)
      | h :: _ when h * prev < 0 -> is_valid t h  (* Zero must be surrounded by opposite signs *)
      | _ -> false
    )
  | x :: t when x <> 0 -> 
      if prev = 0 || x * prev > 0 then is_valid t x  (* Continue if the same sign or if first non-zero *)
      else false
  | _ :: _ -> false  (* Invalid case *)

(* Helper function to group adjacent non-zero integers *)
let rec grouping lst current_group groups =
  match lst with
  | [] -> if current_group = [] then List.rev groups
          else List.rev (List.rev current_group :: groups)  (* End of list, return groups *)
  | 0 :: t -> if current_group = [] then grouping t [] groups
              else grouping t [] (List.rev current_group :: groups)  (* Split at zero *)
  | x :: t -> grouping t (x :: current_group) groups  (* Add non-zero to current group *)

(* Main function *)
let group lst =
  if is_valid lst 0 then
    Some (grouping lst [] [])
  else
    None

