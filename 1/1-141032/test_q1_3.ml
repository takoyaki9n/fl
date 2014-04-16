#use "q1_3.ml";;
  
twice (fun x -> 2*x) 3;;

List.map (fun n -> repeat (fun x -> 2*x) n 3) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;
