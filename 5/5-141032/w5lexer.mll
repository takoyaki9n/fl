{
  open W5parser
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule token = parse
| space+      { token lexbuf }

| "let"       { LET }
| "rec"       { REC }
| "and"       { DAND }
| "in"        { IN  }
| "if"        { IF  }
| "then"      { THEN }
| "else"      { ELSE }

| "match"     { MATCH }
| "with"      { WITH }

| "fun"       { FUN }

| "true"      { TRUE }
| "false"     { FALSE }

| "="         { EQ }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { TIMES }
| '/'         { DIV }

| "->"        { ARROW } 

| "|"         { BAR } 

| '<'         { LT }
| '>'         { GT } (* Extension, see README. *) 
| "<="        { LE } (* Extension, see README. *) 
| ">="        { GE } (* Extension, see README. *) 

| "||"        { OR }  (* Extension, see README. *) 
| "&&"        { AND } (* Extension, see README. *) 

| "::"        { CONS }
| ";;"        { EOC } 

| '('         { LPAR }
| ','         { COMMA }
| ')'         { RPAR }

| '['         { LBRACKET }
| ';'         { SEMICOLON }
| ']'         { RBRACKET }

| digit+ as n { INT (int_of_string n) }
| ident  as n { ID n }
| eof         { EOF  }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
