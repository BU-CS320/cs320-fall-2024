let sum_of_squares = fun x -> fun y ->
  let x_squared = x * x in
  let y_squared = y * y in
  x_squared + y_squared
in sum_of_squares 3 (-5)
