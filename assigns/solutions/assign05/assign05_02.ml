
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let sum_tr t =
  let rec go t cont =
    match t with
    | Leaf -> cont 0
    | Node (x, l, r) ->
        go l (fun al ->
          go r (fun ar ->
            cont (x + al + ar)))
  in go t (fun x -> x)
