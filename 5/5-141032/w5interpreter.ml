open Syntax

exception Eval_error of string;;
  
let empty_env = [];;

let rec eval_expr env = function
  | EConst v -> v
  | EVar (Name v) -> 
     (try
	 List.assoc v env
       with
       |Not_found -> raise (Eval_error "unbound variable"))
  | EFun (x, e) -> VFun (x, e, env)
  | EAdd (e1, e2) -> 
     (match (eval_expr env e1), (eval_expr env e2) with
      | VInt v1, VInt v2 -> VInt (v1 + v2)
      | _ -> raise (Eval_error "add: arguments must be (int, int)"))
  | ESub (e1, e2) -> 
     (match (eval_expr env e1), (eval_expr env e2) with
      | VInt v1, VInt v2 -> VInt (v1 - v2)
      | _ -> raise (Eval_error "sub: arguments must be (int, int)"))
  | EMul (e1, e2) -> 
     (match (eval_expr env e1), (eval_expr env e2) with
      | VInt v1, VInt v2 -> VInt (v1 * v2)
      | _ -> raise (Eval_error "mul: arguments must be (int, int)"))
  | EDiv (e1, e2) -> 
     (match (eval_expr env e1), (eval_expr env e2) with
      | VInt v1, VInt 0 -> raise (Eval_error "div: division by zero")
      | VInt v1, VInt v2 -> VInt (v1 / v2)
      | _ -> raise (Eval_error "div: arguments must be (int, int)"))
  | EEq (e1, e2) -> 
     (match (eval_expr env e1), (eval_expr env e2) with
      | VInt v1, VInt v2 -> VBool (v1 = v2)
      | VBool v1, VBool v2 -> VBool ((v1 && v2) || (not (v1 || v2)))
      | _ -> raise (Eval_error "equal: arguments must be (int, int) or (bool, bool)"))
  | ELT (e1, e2) -> 
     (match (eval_expr env e1), (eval_expr env e2) with
      | VInt v1, VInt v2 -> VBool (v1 < v2)
      | _ -> raise (Eval_error "equal: arguments must be (int, int)"))
  | EIf (e1, e2, e3) -> 
     (match (eval_expr env e1) with
      | VBool v1  -> if v1 then (eval_expr env e2) else (eval_expr env e3)
      | _ -> raise (Eval_error "if: arguments must be (bool, value, value)"))
  | ELet (Name k, e1, e2) ->
     let v1 = eval_expr env e1 in
     let env = (k, v1)::env in
     eval_expr env e2
  | EApp (e1, e2) ->
     (match (eval_expr env e1) with
      | VFun (Name k, b, e) ->  
	 let v = eval_expr env e2 in
	 let e = (k, v)::e in
	 eval_expr e b
      | _ -> raise (Eval_error "app: applying to not a function"))
  | _ -> raise (Eval_error "eval failed");;

