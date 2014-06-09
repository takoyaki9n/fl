let rec map f xs = match xs with | [] -> [] | y::ys -> (f y)::(map f ys) in map (fun x -> x * 2) [1;2;3] 
