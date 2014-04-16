let rec sum_to n =
  match n with
  | 1 -> 1
  | m -> m + sum_to (m - 1);;
  
let is_prime n =
  let rec is_prime_rec n m =
    if n <= m then
      true
    else
      if n mod m = 0 then
	false
      else
	is_prime_rec n (m + 1) 
  in
  if n < 2 then
    false
  else
    is_prime_rec n 2;;

let rec gcd m n =
  if n = 0 then
    m
  else
    gcd n (m mod n);;
