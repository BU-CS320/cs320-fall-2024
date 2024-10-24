
type tok =
  | TNum of int
  | TAdd
  | TLt
  | TIte

let tok_of_string_opt = function
  | "+" -> Some TAdd
  | "<" -> Some TLt
  | "?" -> Some TIte
  | s ->
    s
    |> int_of_string_opt
    |> Option.map (fun n -> TNum n)

let is_whitespace c = List.mem c [' ';'\n';'\t';'\r']

let split s =
  let open List in
  let rec go cs =
    let w = take_while (fun x -> not (is_whitespace x)) cs in
    let cs = drop_while (fun x -> not (is_whitespace x)) cs in
    match w with
    | [] -> []
    | _ -> String.of_list w :: go (drop_while is_whitespace cs)
  in
  s
  |> String.to_list
  |> drop_while is_whitespace
  |> go

type expr =
  | Num of int
  | Add of expr * expr
  | Lt of expr * expr
  | Ite of expr * expr * expr

type ty =
  | TInt
  | TBool

type value =
  | VNum of int
  | VBool of bool
