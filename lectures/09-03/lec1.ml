let x = 3;;

let y = 4;;

let z = x + y;;

let a = x * y;;

let f x = x + 1;;

f 2;;

let g x y z = if x then y else z;;

g true 10 15;;

let h x y z = if x then f y else f z;;

let rec fib n = if n = 0 || n = 1 then 1 else fib (n-1) + fib (n-2);;

fib 5;;