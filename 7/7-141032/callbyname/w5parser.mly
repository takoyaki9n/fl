%{
  open Syntax 

  let mytrue  = EConst (VBool true)
  let myfalse = EConst (VBool false)

  let mynot e = EIf (e,myfalse,mytrue)
  let myand e1 e2 = EIf (e1, e2, myfalse) 
  let myor e1 e2  = EIf (e1, mytrue, e2)
%}

%token <int>    INT
%token <string> ID 
%token EQ 
%token LET REC DAND IN
%token IF THEN ELSE 
%token MATCH WITH BAR  
%token FUN ARROW 
%token TRUE FALSE 
%token PLUS MINUS 
%token TIMES DIV 
%token CONS LBRACKET RBRACKET SEMICOLON
%token OR
%token AND 
%token LT LE GT GE 
%token LPAR RPAR COMMA
%token EOC
%token EOF 

%start command main_expr 
%type <Syntax.command> command
%type <Syntax.expr>    main_expr 

%%

command:
| LET var EQ expr EOC { CLet($2,$4) }
| LET REC letrecs EOC { CRLets $3 }
| expr EOC            { CExp $1 }
| EOF                 { CQuit }
;

pat:
| simple_pat CONS pat { PCons ($1,$3) }
| LPAR tup_pat_inner RPAR { PTup($2) }
| simple_pat          { $1 } 
;

tup_pat_inner:
| pat COMMA tup_pat_inner { $1::$3 }
| pat                 { [$1] }
;
  
simple_pat:
| var           { PVar $1 }
| INT           { PConst (VInt $1) }
| TRUE          { PConst (VBool true) }
| FALSE         { PConst (VBool false) }
| nil           { PNil }
| LPAR pat RPAR { $2 }
;

main_expr: 
| expr EOF { $1 }
;

expr:
| MATCH expr WITH alts         { EMatch($2,$4) }
| LET var EQ expr IN expr      { ELet($2,$4,$6) }
| LET REC letrecs IN expr      { ERLets ($3,$5) }
| IF  expr THEN expr ELSE expr { EIf($2,$4,$6) }
| FUN var args ARROW expr      { EFun ($2, List.fold_right (fun a r -> EFun (a,r)) $3 $5) }
| expr1                        { $1 }
;

exprm: 
| LET var EQ expr IN exprm      { ELet($2,$4,$6) }
| LET REC letrecs IN exprm      { ERLets ($3,$5) }
| IF  expr THEN expr ELSE exprm { EIf($2,$4,$6) }
| FUN var args ARROW exprm      { EFun ($2, List.fold_right (fun a r -> EFun (a,r)) $3 $5) }
| expr1                         { $1 }
;

letrecs:
| letrec               { [$1] }
| letrec DAND letrecs  { $1::$3 }
;

letrec:
| var var args EQ expr { ($1, EFun ($2, List.fold_right (fun a r -> EFun (a,r)) $3 $5)) }
| var EQ expr { ($1, $3) }
;

args:
| var args { $1 :: $2 }
|          { [] }
; 

alts:
| BAR alts1 { $2 }
| alts1     { $1 }
;

alts1:
| alt0 BAR alts1 { $1 :: $3 }
| alt            { [$1] }
;

alt:
| pat ARROW expr  { ($1,$3) }
;

alt0:
| pat ARROW exprm { ($1,$3) }
;

var:
| ID { Name $1 }
;

expr1: 
| expr2 OR expr2 { myor $1 $3 }
| expr2          { $1 }
;

expr2: 
| expr3 AND expr3 { myand $1 $3 }
| expr3           { $1 }
;

expr3:
| expr4 GT expr4    { ELT ($3,$1) }
| expr4 GE expr4    { mynot (ELT ($1,$3)) }
| expr4 LT expr4    { ELT ($1,$3) }
| expr4 LE expr4    { mynot (ELT ($3,$1)) }
| expr4 EQ expr4    { EEq ($1,$3) } 
| expr4             { $1 }
;

expr4:
| expr5 CONS expr4 { ECons ($1,$3) }
| expr5            { $1 }
;

expr5: 
| expr5 PLUS  expr6 { EAdd ($1,$3) }
| expr5 MINUS expr6 { ESub ($1,$3) }
| expr6             { $1 }
;

expr6:
| expr6 TIMES expr7 { EMul ($1,$3) }
| expr6 DIV   expr7 { EDiv ($1,$3) }
| expr7             { $1 }
;

expr7:
| app_expr          { $1 }
| MINUS simple_expr { ESub (EConst (VInt 0), $2) }
; 

app_expr:
| app_expr simple_expr { EApp ($1,$2) }
| simple_expr          { $1 }
;
                        
simple_expr:
| INT               { EConst (VInt $1) }
| ID                { EVar   (Name $1) }
| TRUE              { mytrue }
| FALSE             { myfalse } 
| LPAR expr RPAR    { $2 }
| list_expr         { $1 }
| tup_expr          { $1 }
| nil               { ENil }
;

list_expr:
| LBRACKET list_inner RBRACKET { $2 }
;

list_inner:
| expr SEMICOLON list_inner { ECons($1, $3) }
| expr                      { ECons($1, ENil) }
;

tup_expr:
| LPAR tup_inner RPAR { ETup($2) }
;

tup_inner:
| expr COMMA tup_inner { $1::$3 }
| expr                 { [$1] }
;

nil: 
| LBRACKET RBRACKET {}
;




