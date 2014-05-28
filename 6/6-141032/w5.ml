open Syntax
open W5printer
open W5parser
open W5lexer
open W5interpreter

let read_and_print env f =
  try 
    print_string "> "; flush stdout; 
    let lexbuf = Lexing.from_channel stdin in 
    let result = W5parser.command W5lexer.token lexbuf in 
    (* print_command result; *)
    (match result with 
     | CLet (n, e) ->
     	let v  = (eval_expr env e) in
     	print_result (Some n) v; f ((n, v)::env)
     | CRLets lets ->
     	let envr = ref env in
	envr := List.fold_right (fun (n1, n2, ex) ev -> 
				 let v = VRFun (n2, ex, envr) in
				 print_result (Some n1) v;
				 (n1, v)::ev) lets env ;
     	f !envr
     | CExp e -> print_result None (eval_expr env e); f env
     | CQuit -> print_string "Bye\n"
     | _     -> f env)
  with 
  | Parsing.Parse_error -> 
     print_endline "ParseError!"; f env
  | W5interpreter.Eval_error m ->
     print_endline "EvalError! ";
     print_endline m; f env
  | Failure e -> 
     print_endline ("Failure: " ^ e); f env
					
let rec read_print_loop env =
  read_and_print env read_print_loop 
		 
let read_print_from_channel input =
  try 
    let lexbuf = Lexing.from_channel input in 
    let result = W5parser.main_expr W5lexer.token lexbuf in
    (* print_expr result; *)
    print_result None (eval_expr Syntax.empty_env result); 
  with 
  | Parsing.Parse_error -> 
     print_endline "Parse Error!"
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
    read_print_loop Syntax.empty_env

;;
if !Sys.interactive then 
  ()
else 
  main ()
