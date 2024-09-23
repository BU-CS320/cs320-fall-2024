let sqrt n =
  let rec go i =
    if n <= i * i
    then i
    else go (i + 1)
  in go 0
