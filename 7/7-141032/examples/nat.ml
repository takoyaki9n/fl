let rec take n xs = 
  if n < 1 then
    []
  else
    match xs with
    | [] -> []
    | y::ys -> y::(take (n - 1) ys)
and map f xs = 
  match xs with 
  | [] -> []
  | y::ys -> (f y)::(map f ys)
and nats = 1::(map (fun x -> x + 1) nats) in
    take 10 nats
