open Syntax

exception Eval_error of string;;
  
let empty_env: env = [];;

let rec matching v = function
  | PConst u ->
     if u = v then Some empty_env else None
  | PVar n ->
     Some [(n, v)]
  | _ -> raise (Eval_error "invalid pattern");;
  
let rec find_match v = function
  | [] -> raise (Eval_error "match failure")
  | (pat, ex)::cases -> 
     (match (matching v pat) with
      | None -> find_match v cases
      | Some bnd -> (bnd, ex));;
       
let rec eval_expr env = function
  | EConst v -> v
  | EVar (Name v) -> 
     (try
	 List.assoc (Name v) env
       with
       |Not_found -> raise (Eval_error ("unbound variable " ^ v)))
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
  | ELet (n, e1, e2) ->
     let v1 = eval_expr env e1 in
     let env = (n, v1)::env in
     eval_expr env e2
  | ERLets (lets, e) ->
     let envr = ref env in
     envr := List.fold_right (fun (n1, n2, ex) ev -> (n1, VRFun (n2, ex, envr))::ev) lets env ;
     eval_expr !envr e
  | EApp (e1, e2) ->
     (match (eval_expr env e1) with
      | VFun (n, b, e) -> 
	 let v = eval_expr env e2 in
	 let e = (n, v)::e in
	 eval_expr e b
      | VRFun (n, b, er) -> 
	 let v = eval_expr env e2 in
	 let e = (n, v)::!er in
	 eval_expr e b
      | _ -> raise (Eval_error "app: applying to not a function"))
  | EMatch (e, cases) ->
     let v = eval_expr env e in
     let (bnd, ex) = find_match v cases in
     eval_expr (bnd @ env) ex
  | _ -> raise (Eval_error "unsupported expression");;
  
