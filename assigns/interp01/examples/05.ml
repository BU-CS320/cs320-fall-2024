let omega = fun x -> x x in
let do_not_compute = fun _ -> omega omega in
()
