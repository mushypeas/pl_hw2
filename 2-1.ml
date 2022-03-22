
let identity x = x
let compose f g x = f (g x)

let rec iter (n,f) =
  if n > 0 then compose f (iter ((n-1), f))
  else identity