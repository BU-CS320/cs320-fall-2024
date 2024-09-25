open OUnit2
open Assign03_03

let rec height t =
  match t with
  | Leaf _ -> 0
  | Node cs ->
     let rec max_depth cs =
       match cs with
       | [] -> -1
       | c :: cs -> max (height c) (max_depth cs)
     in 1 + max_depth cs

let check_height t t_name h =
  let d =
    String.concat " "
      [ "height of"
      ; t_name
      ; "is"
      ; string_of_int h
      ]
  in d >:: (fun _ -> assert_equal (height t) h)

let test ht tr tr_name e =
  let d =
    String.concat " "
      [ "testing collapse"
      ; tr_name
      ; string_of_int ht
      ]
  in
  let a = collapse ht tr in
  let t _ = assert_equal e a in
  d >:: t

let t1 =
  Node
    [ Leaf 0
    ; Node [ Leaf 1 ]
    ; Node []
    ]

let t1_1 =
  Node
    [ Leaf 0
    ; Leaf 1
    ; Node []
    ]

let t2 =
  Node
    [ Node
        [ Node
            [ Leaf 1
            ; Leaf 2
            ; Leaf 3
            ]
        ; Node
            [ Leaf 4
            ; Leaf 5
            ]
        ; Node []
        ]
    ; Leaf 6
    ; Node
        [ Leaf 7
        ; Node []
        ]
    ; Leaf 8
    ]

let t2_2 =
  Node
    [ Node
        [ Leaf 1
        ; Leaf 2
        ; Leaf 3
        ; Leaf 4
        ; Leaf 5
        ; Node []
        ]
    ; Leaf 6
    ; Node
        [ Leaf 7
        ; Node []
        ]
    ; Leaf 8
    ]

let t2_1 =
  Node
    [ Leaf 1
    ; Leaf 2
    ; Leaf 3
    ; Leaf 4
    ; Leaf 5
    ; Node []
    ; Leaf 6
    ; Leaf 7
    ; Node []
    ; Leaf 8
    ]

let t3 =
  Node
    [ Node
        [ Node
            [ Node
                [ Node
                    [ Node
                        [ Leaf 1
                        ]
                    ]
                ]
            ; Node []
            ]
        ]
    ]

let t3_5 =
  Node
    [ Node
        [ Node
            [ Node
                [ Node
                    [ Leaf 1
                    ]
                ]
            ; Node []
            ]
        ]
    ]

let t3_4 =
  Node
    [ Node
        [ Node
            [ Node
                [ Leaf 1
                ]
            ; Node []
            ]
        ]
    ]

let t3_3 =
  Node
    [ Node
        [ Node
            [ Leaf 1
            ; Node []
            ]
        ]
    ]

let t3_2 =
  Node
    [ Node
        [ Leaf 1
        ; Node []
        ]
    ]

let t3_1 =
  Node
    [ Leaf 1
    ; Node []
    ]


let tests =
  "basic collapse examples" >:::
    [ test 2 t1 "t1" t1
    ; test 1 t1 "t1" t1_1
    ; test 3 t2 "t2" t2
    ; test 2 t2 "t2" t2_2
    ; test 1 t2 "t2" t2_1
    ; test 6 t3 "t3" t3
    ; test 5 t3 "t3" t3_5
    ; test 4 t3 "t3" t3_4
    ; test 3 t3 "t3" t3_3
    ; test 2 t3 "t3" t3_2
    ; test 1 t3 "t3" t3_1
    ]
