let rec pow n k =
  if k = 0 then 1
  else if k = 1 then n
  else n * pow n (k - 1)
