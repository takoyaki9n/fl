#use "q3_3.ml";;

module IntSet = OrderSet(OrderedInt);;

let data = [10; 4; 9; 6; 11; 2; 14; 13; 0; 5; 8; 7; 1; 12; 3];;
let s = IntSet.load data IntSet.empty;;
IntSet.show s;;
IntSet.size s;;
IntSet.mem 6 s;;
let s = IntSet.remove 1 s;;
IntSet.show s;;
IntSet.mem 6 s;;

