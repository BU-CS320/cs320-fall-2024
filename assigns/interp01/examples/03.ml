let app = fun f -> fun x -> fun y -> f x y in
let add = fun a -> fun b -> a mod b + b mod a in
app add 30
