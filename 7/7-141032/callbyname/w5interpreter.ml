open Syntax

exception Eval_error of string;;
exception Type_error of string;;

let rec eval_expr env = function
  | EConst v -> v
  | EVar (Name v) -> 
     (try
	 let Env cnt = env in
	 let (ex, evr) = List.assoc (Name v) cnt in
	 eval_expr !evr ex
       with
       |Not_found -> raise (Eval_error ("unbound variable " ^ v)))
  | EFun (x, e) -> VFun (x, e, env)
  | ENil -> VNil
  | ECons (e1, e2) -> VCons ((e1, ref env), (e2, ref env))
  | ETup l -> VTup (List.map (fun ex -> (ex, ref env)) l)
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
     let env = add_env n (e1, ref env) env in
     eval_expr env e2
  | ERLets (lets, e) ->
     let envr = ref env in
     envr := List.fold_right (fun (n, ex) ev -> add_env n (ex, envr) ev) lets env;
     eval_expr !envr e
  | EApp (e1, e2) ->
     (match (eval_expr env e1) with
      | VFun (n, b, e) -> 
	 let e = add_env n (e2, ref env) e in
	 eval_expr e b
      | _ -> raise (Eval_error "app: applying to not a function"))
  | EMatch (ex, cases) ->
     let (bnd, ex) = find_match (ex, ref env) cases in
     eval_expr (cat_env bnd env) ex
  | _ -> raise (Eval_error "unsupported expression")

and add_env name thunk env =
  let Env cnt = env in
  Env ((name, thunk)::cnt)

and cat_env env1 env2 =
  let Env cnt1 = env1 in
  let Env cnt2 = env2 in
  Env (cnt1 @ cnt2)

and find_match thk = function
  | [] -> raise (Eval_error "match failure")
  | (pat, ex)::cases ->
     (match (matching thk pat) with
      | None -> find_match thk cases
      | Some bnd -> (bnd, ex))

and matching thk p =
  let ex, evr = thk in
  match p with
  | PConst c ->
     let v = eval_expr !evr ex in
     if v = c then Some empty_env else None
  | PVar n ->
     Some (add_env n thk empty_env)
  | PNil ->
     let v = eval_expr !evr ex in
     if v = VNil then Some empty_env else None
  | PCons (p1, p2) ->
     (match (eval_expr !evr ex) with
      | VCons (t1, t2) -> 
	 (match (matching t1 p1), (matching t2 p2) with
	  | Some bnd1, Some bnd2 -> Some (cat_env bnd1 bnd2)
	  | _, _ -> None)
      | _ -> None)
  | PTup pats ->
     (match (eval_expr !evr ex) with
      | VTup thks -> match_tup thks pats
      | _ -> None)
  | _ -> None

and match_tup thks pats =
  match thks, pats with
  | [], [] -> 
     Some Syntax.empty_env
  | thk::thks, pat::pats -> 
     match (matching thk pat), (match_tup thks pats) with
     | Some bnd1, Some bnd2 -> Some (cat_env bnd1 bnd2)
     | _, _ -> None;;
  
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
  | TList t -> TList (ty_sbst maps t)
  | TTup l -> TTup (List.map (fun t -> ty_sbst maps t) l)
  | _ -> raise (Type_error "unknown type") ;;

let rec appears t u = 
  if t = u then 
    true
  else 
    match u with 
    | TFun (u1, u2) -> (appears t u1) || (appears t u2)
    | TList u -> appears t u
    | TTup l -> List.fold_left (fun b u -> (appears t u) || b) false l
    | _ -> false;;

let rec ty_replace s t u = 
  if s = u then 
    t
  else 
    match u with 
    | TFun (u1, u2) -> TFun(ty_replace s t u1, ty_replace s t u2)
    | TList u -> TList (ty_replace s t u)
    | TTup l -> TTup (List.map (fun u -> ty_replace s t u) l)
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
       | TList t1, TList t2 ->
	  ty_unify ((t1, t2)::conds)
       | TTup l1, TTup l2 ->
	  ty_unify (List.fold_right2 (fun t1 t2 cnds -> (t1, t2)::cnds) l1 l2 conds)
       | TVar v, _ ->
	  if (appears s t) then
	    raise (Type_error "recursive type")
	  else
	    let cnds = List.map (fun (u1, u2) -> (ty_replace s t u1, ty_replace s t u2)) conds in
	    let maps = ty_unify cnds in
	    (v, ty_sbst maps t)::maps
       | _, TVar v ->
	  if (appears t s) then
	    raise (Type_error "recursive type")
	  else
	    let cnds = List.map (fun (u1, u2) -> (ty_replace t s u1, ty_replace t s u2)) conds in
	    let maps = ty_unify cnds in
	    (v, ty_sbst maps s)::maps
       | _, _ -> raise (Type_error "type unmatch")

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
       |Not_found -> raise (Type_error ("unbound variable " ^ v)))
  | EFun (x, e) -> 
     let a = TVar (new_tvar ()) in
     let (t, c) = gather_constraints ((x, a)::tenv) e in
     (TFun (a, t), c)
  | ENil -> (TList (TVar (new_tvar ())), [])
  | ECons (e1, e2) ->
     let (t1, c1) = gather_constraints tenv e1 in
     let (t2, c2) = gather_constraints tenv e2 in
     (TList t1, (t2, TList t1)::(c1 @ c2))
  | ETup l -> 
     let (ts, c) = List.fold_right 
		     (fun e (ts, cs) ->
		      let (t, c) = gather_constraints tenv e in 
		      (t::ts, c @ cs)) l ([], [])in
     (TTup ts, c)
  | EAdd (e1, e2) 
  | ESub (e1, e2) 
  | EMul (e1, e2) 
  | EDiv (e1, e2) -> 
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
  		  (fun (n, e) ev ->
  		   (n, TVar (new_tvar ()))::ev)
  		  lets tenv in
     let conds = List.fold_right
  		   (fun (n, e) conds ->
		    let a = List.assoc n tenv in
  		    let (t, c) = gather_constraints tenv e in
  		    (t, a)::(c @ conds)) lets [] in
     let (t, c) = gather_constraints tenv e in
     (t, conds @ c)
  | EApp (e1, e2) ->
     let (t1, c1) = gather_constraints tenv e1 in
     let (t2, c2) = gather_constraints tenv e2 in
     let a = TVar (new_tvar ()) in
     (a, (t1, TFun(t2, a))::(c1 @ c2))
  | EMatch (e, cases) ->
     let (t, c) = gather_constraints tenv e in
     let a = TVar (new_tvar ()) in
     let conds = List.fold_right
		   (fun (p, ex) cnds -> 
		    let (ti, ci, ei) = gather_constraints_pattern p in
		    let (ti', ci')  = gather_constraints (ei @ tenv) ex in
		    (t, ti)::(a, ti')::(ci @ ci' @ cnds)) cases [] in
     (a, conds)
  | _ -> raise (Type_error "unsupported expression")

and gather_constraints_pattern p = 
  match p with
  | PConst (VInt _) -> 
     (TInt, [], [])
  | PVar n -> 
     let a = TVar (new_tvar ()) in
     (a, [], [(n, a)])
  | PNil -> 
     let a = TVar (new_tvar ()) in
     (TList a, [], [])
  | PCons (p1, p2) ->
     let (t1, c1, e1) = gather_constraints_pattern p1 in
     let (t2, c2, e2) = gather_constraints_pattern p2 in
     let a = TVar (new_tvar ()) in
     (TList a, (a, t1)::(TList a, t2)::(c1 @ c2), e1 @ e2)
  | PTup ps ->
     let (ts, c, e) = List.fold_right 
			(fun p (ts, cs, es) ->
			 let (t, c, e) = gather_constraints_pattern p in 
			 (t::ts, c @ cs, e @ es)) ps ([], [], []) in
     (TTup ts, c, e)
  | _ -> raise (Type_error "match failure")

let infer_expr tenv expr = 
  let (t, conds) = gather_constraints tenv expr in
  let maps =ty_unify conds in
  ty_sbst maps t;;

