let rec fix f x = f (fix f) x;;

let sum_to_by_fix n =
  fix (fun f n ->
       if n < 1 then
	 0
       else
	 n + f (n - 1))n;;
  
let is_prime_by_fix n =
  if n < 2 then
    false
  else
    fix (fun f n m ->
	 if n <= m then
	   true
	 else
	   if n mod m = 0 then
	     false
	   else
	     f n (m + 1)) n 2;;

let gcd_by_fix m n =
  fix(fun f m n ->
      if n = 0 then
	m
      else
	f n (m mod n)) m n;;

let fib1_by_fix n = 
  fix (fun f n -> 
      if n < 2 then
	1
      else
      (f (n - 1)) + (f (n - 2))) n;;

let rec fib2_by_fix n =
  fix (fun f n a b ->
       if n < 1 then
         a + b
       else
	 f (n - 1) (a + b) a) n 0 1;;
