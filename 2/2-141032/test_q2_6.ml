#use "q2_6.ml";;

(* 
if (3 + 6) = 9 then 
  24 / (if 5 < 0 then 
	  8 
	else 
	  6) 
else 
  3 * 4;;
*)

let f = EIf (
	    EEq (
		EAdd (
		    EConst (VInt 3),
		    EConst (VInt 6)),
		EConst (VInt 9)),
	    EDiv (
		EConst (VInt 24),
		EIf (
		    ELt (
			EConst (VInt 5),
			EConst (VInt 0)),
		    EConst (VInt 8),
		    EConst (VInt 6))),
	    EMul (
		EConst (VInt 3),
		EConst (VInt 4)))
    in
    eval f;;
