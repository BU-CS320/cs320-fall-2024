open OUnit2
open Assign05_04

let l0 = [0; -1; 23; 5435; 2]
let l1 = [0; -2; 1; 23; 3]

module TestList = struct
  open ListSet

  let rec of_list l =
    match l with
    | [] -> empty
    | [x] -> singleton x
    | x :: l -> union (singleton x) (of_list l)

  let to_list s = s

  let t_card_empty =
    "card of empty" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 0 (card empty))

  let t_card_single =
    "card of singleton" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 1 (card (singleton 13)))

  let t_card_l0 =
    "card of l0" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 5 (card (of_list l0)))

  let t_mem_empty =
    "mem of empty" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_bool "empty has a member" (not (mem 10 empty)))

  let t_mem_single =
    "mem of single true" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_bool "singleton does not have element" (mem 10 (singleton 10)))

  let t_mem_single_false =
    "mem of single false" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_bool "single has wrong element" (not (mem 13 (singleton 10))))

  let t_mn =
    "mn value correct" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal (-1) (List.nth (of_list l0) 0))

  let t_mx =
    "mx value correct" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 5435 (List.nth (of_list l0) 4))

  let t_union =
    "testing union" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal [-2; -1; 0; 1; 2; 3; 23; 5435] (union (of_list l0) (of_list l1)))

  let basic_examples =
    "basic ListSet examples" >:::
      [ t_card_empty
      ; t_card_single
      ; t_card_l0
      ; t_mem_empty
      ; t_mem_single
      ; t_mem_single_false
      ; t_mn
      ; t_mx
      ; t_union
      ]
end

module TestFunc = struct
  open FuncSet

  let rec of_list l =
    match l with
    | [] -> empty
    | [x] -> singleton x
    | x :: l -> union (singleton x) (of_list l)

  let to_list s =
    let rec go i acc =
      if i < s.mn
      then acc
      else if s.ind i
      then go (i - 1) (i :: acc)
      else go (i - 1) acc
    in go s.mx []

  let t_card_empty =
    "card of empty" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 0 (card empty))

  let t_card_single =
    "card of singleton" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 1 (card (singleton 13)))

  let t_card_l0 =
    "card of l0" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 5 (card (of_list l0)))

  let t_mem_empty =
    "mem of empty" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_bool "empty has a member" (not (mem 10 empty)))

  let t_mem_single =
    "mem of single true" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_bool "singleton does not have element" (mem 10 (singleton 10)))

  let t_mem_single_false =
    "mem of single false" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_bool "single has wrong element" (not (mem 13 (singleton 10))))

  let t_mn =
    "mn value correct" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal (-1) (of_list l0).mn)

  let t_mx =
    "mx value correct" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal 5435 (of_list l0).mx)

  let t_union =
    "testing union" >:
      test_case
        ~length:(Custom_length 1.)
        (fun _ -> assert_equal [-2; -1; 0; 1; 2; 3; 23; 5435] (to_list (union (of_list l0) (of_list l1))))

  let basic_examples =
    "basic FuncSet examples" >:::
      [ t_card_empty
      ; t_card_single
      ; t_card_l0
      ; t_mem_empty
      ; t_mem_single
      ; t_mem_single_false
      ; t_mn
      ; t_mx
      ; t_union
      ]
end

let basic_examples = "basic INT_SET examples" >:::
  [ TestList.basic_examples
  ; TestFunc.basic_examples
  ]
