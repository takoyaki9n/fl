let fix = (fun f -> (fun x -> f (fun y -> x x y)) (fun x -> f (fun y -> x x y))) in let rec hfact f n = if n < 2 then 1 else n * (f (n - 1)) in (fix hfact) 6
