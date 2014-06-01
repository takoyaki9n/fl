open Syntax

exception Eval_error of string;;

let rec matching v p =
  match v, p with
  | _, PConst c ->
     if v = c then Some empty_env else None
  | _, PVar n ->
     Some [(n, v)]
  | VList [], PNil -> 
     Some Syntax.empty_env
  | VList (x::xs), PCons (p1, p2) ->
     (match (matching x p1), (matching (VList xs) p2) with 
      | Some bnd1, Some bnd2 -> Some (bnd1 @ bnd2)
      | _, _ -> None)
  | VTup [], PTup [] ->
     Some Syntax.empty_env
  | VTup (x::xs), PTup (y::ys) ->
     (match (matching x y), (matching (VTup xs) (PTup ys)) with 
      | Some bnd1, Some bnd2 -> Some (bnd1 @ bnd2)
      | _, _ -> None)
  | _ -> None;;
  
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
  | ENil -> VList []
  | ECons (e1, e2) -> 
     (match (eval_expr env e1), (eval_expr env e2) with
      | v, VList l -> VList (v::l)
      | _ -> raise (Eval_error "cons: arguments must be (_, list)"))
  | ETup l -> VTup (List.map (eval_expr env) l)
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
  
let rec ty_sbst maps ty = 
  let rec ty_sbst_one mps v =
    match mps with
    | [] -> TVar v
    | (u, t)::ms -> 
       if v = u then t else ty_sbst_one ms v in
  match ty with
  | TInt | TBool -> ty
  | TFun(t1, t2) -> TFun(ty_sbst maps t1, ty_sbst maps t2)
  | TVar v -> ty_sbst_one maps v
  | _ -> raise (Eval_error "unify faled") ;;

let rec appears t u = 
  if t = u then 
    true
  else 
    match u with 
    | TFun (u1, u2) -> (appears t u1) || (appears t u2)
    | _ -> false;;

let rec ty_replace s t u = 
  if s = u then 
    t
  else 
    match u with 
    | TFun (u1, u2) -> TFun(ty_replace s t u1, ty_replace s t u2)
    | _ -> u;;

let rec ty_unify = function
  | [] -> []
  | (s, t)::conds -> 
     if s = t then
       ty_unify conds
     else
       match s, t with
       | TFun(s1, s2), TFun(t1, t2) ->
	  ty_unify ((s1, t1)::(s2, t2)::conds)
       | TVar v, _ ->
	  if (appears s t) then
	    raise (Eval_error "unify failed")
	  else
	    let cnds = List.map (fun (u1, u2) -> (ty_replace s t u1, ty_replace s t u2)) conds in
	    let maps = ty_unify cnds in
	    (v, ty_sbst maps t)::maps
       | _, TVar v ->
	  if (appears t s) then
	    raise (Eval_error "unify failed")
	  else
	    let cnds = List.map (fun (u1, u2) -> (ty_replace t s u1, ty_replace t s u2)) conds in
	    let maps = ty_unify cnds in
	    (v, ty_sbst maps s)::maps
       | _, _ -> raise (Eval_error "unify failed")

let tvar_val = ref 0;;

let new_tvar () = tvar_val := !tvar_val + 1; !tvar_val - 1;;

let rec gather_constraints tenv expr = 
  match expr with
  | EConst (VInt _) -> (TInt, [])
  | EConst (VBool _) -> (TBool, [])
  | EVar (Name v) -> 
     (try
	 (List.assoc (Name v) tenv, [])
       with
       |Not_found -> raise (Eval_error ("unbound variable " ^ v)))
  | EFun (x, e) -> 
     let a = TVar (new_tvar ()) in
     let (t, c) = gather_constraints ((x, a)::tenv) e in
     (TFun (a, t), c)
  (* | ENil -> VList [] *)
  (* | ECons (e1, e2) ->  *)
  (*    (match (eval_expr env e1), (eval_expr env e2) with *)
  (*     | v, VList l -> VList (v::l) *)
  (*     | _ -> raise (Eval_error "cons: arguments must be (_, list)")) *)
  (* | ETup l -> VTup (List.map (eval_expr env) l) *)
  | EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2) -> 
     let (t1, c1) = gather_constraints tenv e1 in
     let (t2, c2) = gather_constraints tenv e2 in
     (TInt, (t1, TInt)::(t2, TInt)::(c1 @ c2))
  | EEq (e1, e2) ->
     let (t1, c1) = gather_constraints tenv e1 in
     let (t2, c2) = gather_constraints tenv e2 in
     (TBool, (t1, t2)::(c1 @ c2))
  | ELT (e1, e2) -> 
     let (t1, c1) = gather_constraints tenv e1 in
     let (t2, c2) = gather_constraints tenv e2 in
     (TBool, (t1, TInt)::(t2, TInt)::(c1 @ c2))
  | EIf (e1, e2, e3) -> 
     let (t1, c1) = gather_constraints tenv e1 in
     let (t2, c2) = gather_constraints tenv e2 in
     let (t3, c3) = gather_constraints tenv e3 in
     (t2, (t1, TBool)::(t2, t3)::(c1 @ c2 @ c3))
  | ELet (n, e1, e2) ->
     let (t1, c1) = gather_constraints tenv e1 in
     let tenv = (n, t1)::tenv in
     let (t2, c2) = gather_constraints tenv e2 in
     (t2, c1@c2)
  | ERLets (lets, e) ->
     let tenv = List.fold_right 
		  (fun (f, x, e) ev -> 
		   (f, TFun(TVar (new_tvar ()), TVar (new_tvar ())))::ev) 
		  lets tenv in
     let conds = List.fold_right 
		   (fun (f, x, e) conds ->
		    let TFun (a, b) = List.assoc f tenv in
		    let (t, c) = gather_constraints ((x, a)::tenv) e in
		    (t, b)::(c @ conds)) lets [] in
     let (t, c) = gather_constraints tenv e in
     (t, conds @ c)
  | EApp (e1, e2) ->
     let (t1, c1) = gather_constraints tenv e1 in
     let (t2, c2) = gather_constraints tenv e2 in
     let a = TVar (new_tvar ()) in
     (a, (t1, TFun(t2, a))::(c1 @ c2))
  (* | EMatch (e, cases) -> *)
  (*    let v = eval_expr env e in *)
  (*    let (bnd, ex) = find_match v cases in *)
  (*    eval_expr (bnd @ env) ex *)
  | _ -> raise (Eval_error "unsupported expression");;

let infer_expr tenv expr = 
  let (t, conds) = gather_constraints tenv expr in
  let maps =ty_unify conds in
  ty_sbst maps t;;
