let f = fun x ->
  if x < 0 then
    -1
  else if x = 0 then
    37
  else if x > 0 && x < 10 then
    true
  else if x = 10 || x > 10 then
    ()
  else
    fun x -> x
in f 7
