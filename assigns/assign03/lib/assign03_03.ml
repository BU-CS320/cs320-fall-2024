type tree = 
  | Leaf of int
  | Node of tree list

(* Helper function to flatten a list *)
let rec flatten lst =
  match lst with
  | [] -> []
  | hd :: tl -> hd @ flatten tl

(* Function to gather terminal elements in a tree *)
let rec get_terminal_elements t =
  match t with
  | Leaf _ -> [t]
  | Node [] -> [t]
  | Node children -> flatten (List.map get_terminal_elements children)

(* Function to collapse the tree to a specified height *)
let rec collapse h t =
  match t with
  | Leaf _ -> t
  | Node [] -> Node []
  | Node children ->
      if h = 1 then
        Node (get_terminal_elements t)  (* Replace with terminal elements *)
      else
        Node (List.map (collapse (h-1)) children)
