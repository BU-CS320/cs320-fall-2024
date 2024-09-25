let rec pow n k =
  if k <= 0
  then 1
  else
    pow n (k / 2)
    * pow n (k / 2)
    * (if k mod 2 = 1 then n else 1)
