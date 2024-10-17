(* ListSet module implementation *)
open Assign05_04_intf  (* Ensure that this refers to the correct interface *)

module ListSet : INT_SET with type t = int list = struct
  type t = int list

  let empty = []

  let mem x set = List.mem x set

  let singleton x = [x]

  let card set = List.length set

  let union set1 set2 = 
    let rec merge lst1 lst2 =
      match lst1, lst2 with
      | [], lst | lst, [] -> lst
      | h1 :: t1, h2 :: t2 ->
        if h1 < h2 then h1 :: merge t1 lst2
        else if h1 > h2 then h2 :: merge lst1 t2
        else h1 :: merge t1 t2
    in
    merge set1 set2
end



(* Define set_info type *)
type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

(* FuncSet module implementation *)
module FuncSet : INT_SET with type t = set_info = struct
  type t = set_info

  let empty = { ind = (fun _ -> false); mn = 1; mx = 0 }

  let mem x set = set.ind x

  let singleton x = { ind = (fun y -> y = x); mn = x; mx = x }

  let card set =
    if set.mn > set.mx then 0
    else
      let rec count acc x =
        if x > set.mx then acc
        else if set.ind x then count (acc + 1) (x + 1)
        else count acc (x + 1)
      in
      count 0 set.mn

  let union set1 set2 = 
    let new_ind y = set1.ind y || set2.ind y in
    let new_mn = min set1.mn set2.mn in
    let new_mx = max set1.mx set2.mx in
    { ind = new_ind; mn = new_mn; mx = new_mx }
end
