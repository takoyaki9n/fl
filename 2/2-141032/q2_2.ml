type bnat = B | O of bnat | I of bnat;;
  
exception Negative_number;;

let rec add n1 n2 =
  match (n1, n2) with
  | (B, m) | (m, B) -> m
  | (O m1, O m2) -> O (add m1 m2)
  | (O m1, I m2) | (I m1, O m2) -> I (add m1 m2)
  | (I m1, I m2) -> O (add (add m1 (I B)) m2);;

let rec sub n1 n2 =
  let n = match (n1, n2) with
    | (_, B) -> n1
    | (B, _) -> raise Negative_number
    | (O m1, O m2) -> O (sub m1 m2)
    | (O m1, I m2) -> I (sub (sub m1 (I B)) m2)
    | (I m1, O m2) -> I (sub m1 m2)
    | (I m1, I m2) -> O (sub m1 m2)
  in
  match n with
  | O B -> B
  | _ -> n;;

let rec mul n1 n2 =
  let n =  match n2 with 
    | B -> B
    | O m -> O (mul n1 m)
    | I m -> add n1 (O (mul n1 m))
  in
  match n with 
    | O B -> B
    | _ -> n;;

let rec pow n1 n2 =
  match n2 with 
  | B -> I B
  | O m -> mul (pow n1 m) (pow n1 m)
  | I m -> mul (mul n1 (pow n1 m)) (pow n1 m);;

let rec n2i n =
  match n with 
  | B -> 0
  | O m -> 2 * (n2i m)
  | I m -> 2 * (n2i m) + 1;;
  
let rec i2n i =
  if i < 0 then
    raise Negative_number
  else
    if i = 0 then
      B
    else
      if (i mod 2) = 0 then
	O (i2n (i lsr 1))
      else
	I (i2n (i lsr 1));;

let test op i1 i2 =
  op (i2n i1) (i2n i2);;
