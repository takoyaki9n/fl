let rec fib1 n =
  if n <= 1 then
    1
  else
    (fib1 (n - 1)) + (fib1 (n - 2));;

let fib2 n =
  let rec fib2_rec n f1 f0 =
    if n <= 1 then
      f1 + f0
    else
      fib2_rec (n - 1) (f1 + f0) f1
  in
  fib2_rec n 1 0;;
