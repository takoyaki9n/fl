open Syntax
open ExampleParser
open ExampleLexer

let main () =
  try 
    let lexbuf = Lexing.from_channel stdin in 
    let result = ExampleParser.main ExampleLexer.token lexbuf in
    print_expr result; print_newline ()
  with 
    | Parsing.Parse_error -> 
      print_endline "Parse Error!"
      
;;
if !Sys.interactive then 
  ()
else 
  main ()    

    
    
  
  
