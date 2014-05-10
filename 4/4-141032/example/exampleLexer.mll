{
  open ExampleParser
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule token = parse
| space+      { token lexbuf }
| "let"       { LET }
| "in"        { IN  }
| "="         { EQ }
| '+'         { PLUS }
| '('         { LPAR }
| ')'         { RPAR }
| digit+ as n { INT (int_of_string n) }
| ident  as n { ID n }
| eof         { EOF  }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
