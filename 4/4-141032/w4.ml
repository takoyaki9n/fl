open Syntax
open W4parser
open W4lexer
open W4interpreter

let read_and_print env f =
  try 
    print_string "> "; flush stdout; 
    let lexbuf = Lexing.from_channel stdin in 
    let result = W4parser.command W4lexer.token lexbuf in 
    (match result with 
     | CLet (Name n, e) ->
     	let v  = (eval_expr env e) in
     	print_variable n v; f ((n, v)::env)
     | CExp e -> print_value (eval_expr env e); f env
     | CQuit -> print_string "Bye\n"
     | _     -> f env)
  with 
  | Parsing.Parse_error -> 
     print_endline "ParseError!"; f env
  | W4interpreter.Eval_error m ->
     print_endline "EvalError!"; f env
  | Failure e -> 
     print_endline ("Failure: " ^ e); f env
					
let rec read_print_loop env =
  read_and_print env read_print_loop 
		 
let read_print_from_channel input =
  try 
    let lexbuf = Lexing.from_channel input in 
    let result = W4parser.main_expr W4lexer.token lexbuf in
    print_value (eval_expr W4interpreter.empty_env result); 
  with 
  | Parsing.Parse_error -> 
     print_endline "Parse Error!"

let main () =
  if Array.length Sys.argv > 1 then 
    if Sys.argv.(1) = "-" then 
      read_print_from_channel stdin 
    else 
      let input  = open_in (Sys.argv.(1)) in 
      read_print_from_channel input; close_in input
  else 
    read_print_loop W4interpreter.empty_env

;;
if !Sys.interactive then 
  ()
else 
  main ()
