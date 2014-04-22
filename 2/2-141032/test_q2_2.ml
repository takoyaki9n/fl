#use "q2_2.ml";;

List.map (fun i -> i2n i) [0; 1; 2; 3; 4; 5];;

List.map (fun n -> n2i n) (List.map (fun i -> i2n i) [0; 1; 2; 3; 4; 5]);;

let test op x1 x2 = 
  n2i (op (i2n x1) (i2n x2));;

test add 3 2;;

test sub 3 2;;

test mul 3 2;;

test pow 3 2;;
