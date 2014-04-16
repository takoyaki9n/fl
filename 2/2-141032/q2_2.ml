type bnat = O_ | I_ | O of bnat | I of bnat;;
  
exception Negative_number;;

let rec add n1 n2 =
  match (n1, n2) with
  | (O_, m) | (m, O_) -> m
  | (I_, I_) -> O I_
  | (I_, O m) | (O m, I_) -> I m
  | (I_, I m) | (I m, I_) -> O (add I_ m)
  | (O m1, O m2) -> O (add m1 m2)
  | (O m1, I m2) | (I m1, O m2) -> I (add m1 m2)
  | (I m1, I m2) -> O (add (add m1 m2) I_);;
  
let rec sub n1 n2 =
  let n = match (n1, n2) with
    | (_, O_) -> n1
    | (O_, _) -> raise Negative_number
    | (I_, I_) -> O_
    | (I_, _) -> raise Negative_number
    | (O m, I_) -> I (sub m I_)
    | (O m1, O m2) -> O (sub m1 m2)
    | (O m1, I m2) -> I (sub (sub m1 I_) m2)
    | (I m, I_) -> O m
    | (I m1, O m2) -> I (sub m1 m2)
    | (I m1, I m2) -> O (sub m1 m2)
  in
  match n with
  | O O_ -> O_
  | I O_ -> I_
  | _ -> n;;
  
let rec mul n1 n2 =
  match n2 with 
  | O_ -> O_
  | I_ -> n1
  | O m -> O (mul n1 m)
  | I m -> add n1 (O (mul n1 m));;

let rec pow n1 n2 =
  match n2 with 
  | O_ -> I_
  | I_ -> n1
  | O m -> mul (pow n1 m) (pow n1 m)
  | I m -> mul (mul n1 (pow n1 m)) (pow n1 m);;
  
n2i (test pow 2 7);;

let rec n2i n =
  match n with 
  | O_ -> 0
  | I_ -> 1
  | O m -> 2 * (n2i m)
  | I m -> 2 * (n2i m) + 1;;

let rec i2n i =
  match i with 
  | _ when i < 0 -> raise Negative_number
  | 0 -> O_
  | 1 -> I_
  | _ when (i mod 2) = 0 -> O (i2n (i lsr 1))
  | _ -> I (i2n (i lsr 1));;

let test op i1 i2 =
  op (i2n i1) (i2n i2);;
