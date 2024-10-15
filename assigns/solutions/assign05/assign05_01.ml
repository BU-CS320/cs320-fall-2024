
type 'a test =
  | TestCase of 'a
  | TestList of 'a test list

let fold_left op base t =
  let rec go t x =
    match t with
    | TestCase t -> op x t
    | TestList ts -> List.fold_left (|>) x (List.map go ts) (* this is fancier than it needs to be *)
  in go t base
