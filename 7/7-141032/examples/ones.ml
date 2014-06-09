let rec take n xs = 
  if n < 1 then
    []
  else
    match xs with
    | [] -> []
    | y::ys -> y::(take (n - 1) ys)
and  ones = 1::ones in
    take 10 ones
