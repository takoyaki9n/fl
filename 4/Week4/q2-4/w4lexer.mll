{
  open W4parser
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule token = parse
| space+      { token lexbuf }

| "let"       { LET }
| "in"        { IN  }
| "if"        { IF  }
| "then"      { THEN }
| "else"      { ELSE }

| "true"      { TRUE }
| "false"     { FALSE }

| "="         { EQ }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { TIMES }
| '/'         { DIV }

| '<'         { LT }
| '>'         { GT } (* Extension, see README. *) 
| "<="        { LE } (* Extension, see README. *) 
| ">="        { GE } (* Extension, see README. *) 

| "||"        { OR }  (* Extension, see README. *) 
| "&&"        { AND } (* Extension, see README. *) 

| ";;"        { EOC } 

| '('         { LPAR }
| ')'         { RPAR }
| digit+ as n { INT (int_of_string n) }
| ident  as n { ID n }
| eof         { EOF  }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
