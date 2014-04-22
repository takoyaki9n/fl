type nat = Z | S of nat;;

exception Negative_number;;
  
let rec add n1 n2 = 
  match n2 with 
  | Z -> n1
  | S m -> add (S n1) m;;
  
let rec sub n1 n2 =
  match n2 with 
  | Z -> n1
  | S m2 -> 
     match n1 with
     | Z -> raise Negative_number
     | S m1 -> sub m1 m2;;
  
let rec mul n1 n2 =
  match n2 with
  | Z -> Z
  | S m -> add n1 (mul n1 m);;
  
let rec pow n1 n2 =
  match n2 with
  | Z -> S Z
  | S m -> mul n1 (pow n1 m);;
  
let rec n2i n =
  match n with 
  | Z -> 0
  | S m -> 1 + (n2i m);;
  
let rec i2n i =
  if i < 0 then
    raise Negative_number
  else
    if i = 0 then
      Z
    else
      S (i2n (i - 1));;
  
