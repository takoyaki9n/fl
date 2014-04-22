#use "q2_5.ml";;

exception Eval_error of string;;
  
let rec eval e = 
  match e with
  | EConst v -> v
  | EAdd (e1, e2) -> 
     (match (eval e1), (eval e2) with
      | VInt v1, VInt v2 -> VInt (v1 + v2)
      | _ -> raise (Eval_error "add: arguments must be (int, int)"))
  | ESub (e1, e2) -> 
     (match (eval e1), (eval e2) with
      | VInt v1, VInt v2 -> VInt (v1 - v2)
      | _ -> raise (Eval_error "sub: arguments must be (int, int)"))
  | EMul (e1, e2) -> 
     (match (eval e1), (eval e2) with
      | VInt v1, VInt v2 -> VInt (v1 * v2)
      | _ -> raise (Eval_error "mul: arguments must be (int, int)"))
  | EDiv (e1, e2) -> 
     (match (eval e1), (eval e2) with
      | VInt v1, VInt 0 -> raise (Eval_error "div: division by zero")
      | VInt v1, VInt v2 -> VInt (v1 / v2)
      | _ -> raise (Eval_error "div: arguments must be (int, int)"))
  | EEq (e1, e2) -> 
     (match (eval e1), (eval e2) with
      | VInt v1, VInt v2 -> VBool (v1 = v2)
      | VBool v1, VBool v2 -> VBool ((v1 && v2) || (not (v1 || v2)))
      | _ -> raise (Eval_error "equal: arguments must be (int, int) or (bool, bool)"))
  | ELt (e1, e2) -> 
     (match (eval e1), (eval e2) with
      | VInt v1, VInt v2 -> VBool (v1 < v2)
      | _ -> raise (Eval_error "equal: arguments must be (int, int)"))
  | EIf (e1, e2, e3) -> 
     (match (eval e1) with
      | VBool v1  -> if v1 then (eval e2) else (eval e3)
      | _ -> raise (Eval_error "if: arguments must be (bool, value, value)"));;
