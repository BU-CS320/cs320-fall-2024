module ListSet = struct
  type t = int list

  let mem = List.mem

  let empty = []

  let singleton x = [x]

  let card = List.length

  let union =
    let rec merge l1 l2 =
      match l1, l2 with
      | x :: xs, y :: ys ->
        if x = y
        then x :: merge xs ys
        else if x < y
        then x :: merge xs l2
        else y :: merge l1 ys
      | [], _ -> l2
      | _, [] -> l1
    in merge
end

type set_info =
  { ind : int -> bool
  ; mn : int
  ; mx : int
  }

module FuncSet = struct
  type t = set_info

  let mem x s = s.ind x

  let empty = {ind = (fun _ -> false); mn = 1; mx = 0}

  let singleton x = {ind = (=) x; mn = x; mx = x}

  let card s =
    let rec go acc i =
      if i > s.mx
      then acc
      else go (acc + if s.ind i then 1 else 0) (i + 1)
    in go 0 s.mn

  let union s1 s2 =
    if card s1 = 0
    then s2
    else if card s2 = 0
    then s1
    else
      let ind n = s1.ind n || s2.ind n in
      let mx = max s1.mx s2.mx in
      let mn = min s1.mn s2.mn in
      {ind; mx; mn}
end
