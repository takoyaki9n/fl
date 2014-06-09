let rec take n xs = 
  if n < 1 then
    []
  else
    match xs with
    | [] -> []
    | y::ys -> y::(take (n - 1) ys)
and tail = fun xs -> match xs with x::xs -> xs
and map2 f xs ys = 
  match (xs, ys)with 
  | (x::xs, y::ys) -> (f x y)::(map2 f xs ys)
and fibs = 1::1::(map2 (fun x y -> x + y) fibs (tail fibs)) in
    take 10 fibs
