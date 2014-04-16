#use "q1_4.ml";;

List.map (fun n -> sum_to_by_fix n) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

List.map (fun n -> is_prime_by_fix n) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

gcd_by_fix 105 135;;

List.map (fun n -> fib1_by_fix n) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

List.map (fun n -> fib2_by_fix n) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;
