open Syntax
open W5printer
open W5parser
open W5lexer
open W5interpreter

let read_and_print env tenv f =
  try 
    print_string "> "; flush stdout; 
    let lexbuf = Lexing.from_channel stdin in 
    let result = W5parser.command W5lexer.token lexbuf in 
    print_command result;
    (match result with 
     | CLet (n, e) ->
	let t = infer_expr tenv e in
     	let v = eval_expr env e in
	let env = add_env env (n, (e, env)) in
     	print_result (Some n) v t; f env ((n, t)::tenv)
     (* | CRLets lets -> *)
     (* 	let tenv_tmp = List.fold_right  *)
     (* 			 (fun (f, x, e) ev ->  *)
     (* 			  (f, TFun(TVar (new_tvar ()), TVar (new_tvar ())))::ev)  *)
     (* 			 lets tenv in *)
     (* 	let conds = List.fold_right  *)
     (* 		      (fun (f, x, e) conds -> *)
     (* 		       let TFun (a, b) = List.assoc f tenv_tmp in *)
     (* 		       let (t, c) = gather_constraints ((x, a)::tenv_tmp) e in *)
     (* 		       (t, b)::(c @ conds)) lets [] in *)
     (* 	let maps = ty_unify conds in *)
     (* 	let tenv = List.fold_right  *)
     (* 		     (fun (f, x, e) ev-> (f, ty_sbst maps (List.assoc f tenv_tmp))::ev) lets tenv in *)
     (* 	let envr = ref env in *)
     (* 	envr := List.fold_right (fun (f, x, e) ev ->  *)
     (* 				 let v = VRFun (x, e, envr) in *)
     (* 				 let t = List.assoc f tenv in *)
     (* 				 print_result (Some f) v t; *)
     (* 				 (f, v)::ev) lets env; *)
     (* 	f !envr tenv *)
     | CExp e -> 
	let t = infer_expr tenv e in
     	let v = eval_expr env e in
	print_result None v t; f env tenv
     | CQuit -> print_string "Bye\n"
     | _     -> f env tenv)
  with 
  | Parsing.Parse_error -> 
     print_endline "ParseError!"; f env tenv
  | W5interpreter.Type_error m ->
     print_endline "TypeError! ";
     print_endline m; f env tenv
  | W5interpreter.Eval_error m ->
     print_endline "EvalError! ";
     print_endline m; f env tenv
  | Failure e -> 
     print_endline ("Failure: " ^ e); f env tenv
					
let rec read_print_loop env tenv =
  read_and_print env tenv read_print_loop 
		 
let read_print_from_channel input =
  try 
    let lexbuf = Lexing.from_channel input in 
    let result = W5parser.main_expr W5lexer.token lexbuf in
    let t = infer_expr empty_ty_env result in
    (* print_expr result; *)
    print_result None (eval_expr Syntax.empty_env result) t; 
  with 
  | Parsing.Parse_error -> 
     print_endline "Parse Error!"
  | W5interpreter.Type_error m ->
     print_endline "TypeError! "; print_endline m;
  | W5interpreter.Eval_error m ->
     print_endline "EvalError! "; print_endline m

let main () =
  if Array.length Sys.argv > 1 then 
    if Sys.argv.(1) = "-" then 
      read_print_from_channel stdin 
    else 
      let input  = open_in (Sys.argv.(1)) in 
      read_print_from_channel input; close_in input
  else 
    read_print_loop Syntax.empty_env Syntax.empty_ty_env

;;
if !Sys.interactive then 
  ()
else 
  main ()
