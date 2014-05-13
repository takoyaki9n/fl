open Syntax
open W4parser
open W4lexer

let read_and_print () f =
  try 
    print_string "> "; flush stdout; 
    let lexbuf = Lexing.from_channel stdin in 
    let result = W4parser.command W4lexer.token lexbuf in 
    print_command result; 
    (match result with 
      | CQuit -> ()
      | _     -> f ())
  with 
    | Parsing.Parse_error -> 
      print_endline "ParseError!"; f ()
    | Failure e -> 
      print_endline ("Failure: " ^ e); f ()

let rec read_print_loop () =
  read_and_print () read_print_loop 
    
let read_print_from_channel input =
  try 
    let lexbuf = Lexing.from_channel input in 
    let result = W4parser.main_expr W4lexer.token lexbuf in
    print_expr result
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
    read_print_loop ()
      
;;
if !Sys.interactive then 
  ()
else 
  main ()    

    
    
  
  
