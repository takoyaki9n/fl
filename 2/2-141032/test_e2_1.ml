#use "e2_1.ml";;

List.map (fun i -> c2i (i2c i)) [0; 1; 2; 3; 4; 5];;

let test op x1 x2 = 
  c2i (op (i2c x1) (i2c x2));;

test add 3 2;;

test sub 3 2;;

test mul 3 2;;

test pow 3 2;;
