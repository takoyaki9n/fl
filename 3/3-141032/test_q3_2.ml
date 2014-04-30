#use "q3_2.ml";;

let stk = Stack.empty;;
Stack.size stk;;
let stk = Stack.push 1 stk;;
Stack.size stk;;
let stk = Stack.push 2 stk;;
Stack.size stk;;
let (v, stk) = Stack.pop stk;;
Stack.size stk;;
let (v, stk) = Stack.pop stk;;
Stack.size stk;;
let (v, stk) = Stack.pop stk;;
