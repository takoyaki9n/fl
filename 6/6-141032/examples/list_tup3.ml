let rec sum xs = match xs with | [] -> 0 | y::ys -> y + (sum ys) in sum (1::2::3::4::[])
