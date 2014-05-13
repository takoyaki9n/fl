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
%token LET IN
%token IF THEN ELSE 
%token TRUE FALSE 
%token PLUS MINUS 
%token TIMES DIV 
%token OR
%token AND 
%token LT LE GT GE 
%token LPAR RPAR 
%token EOC
%token EOF 

%start command main_expr 
%type <Syntax.command> command
%type <Syntax.expr>    main_expr 

%%

command:
| LET var EQ expr EOC { CLet($2,$4) }
| expr EOC            { CExp $1 }
| EOF                 { CQuit }
;

main_expr: 
| expr EOF { $1 }
;

expr: 
| LET var EQ expr IN expr      { ELet($2,$4,$6) }
| IF  expr THEN expr ELSE expr { EIf($2,$4,$6) }
| bool_arith_expr              { $1 }
;

var:
| ID { Name $1 }
;

bool_arith_expr: 
| bool_arith_factor_expr OR bool_arith_factor_expr { myor $1 $3 }
| bool_arith_factor_expr                           { $1 }
;

bool_arith_factor_expr: 
| bool_arith_simple_expr AND bool_arith_simple_expr { myand $1 $3 }
| bool_arith_simple_expr                            { $1 }
;

bool_arith_simple_expr: 
| arith_expr GT arith_expr    { ELT ($3,$1) }
| arith_expr GE arith_expr    { mynot (ELT ($1,$3)) }
| arith_expr LT arith_expr    { ELT ($1,$3) }
| arith_expr LE arith_expr    { mynot (ELT ($3,$1)) }
| arith_expr EQ arith_expr    { EEq ($1,$3) } 
| arith_expr                  { $1 }
;

arith_expr: 
| arith_expr PLUS  factor_expr { EAdd ($1,$3) }
| arith_expr MINUS factor_expr { ESub ($1,$3) }
| factor_expr                  { $1 }
;

factor_expr:
| factor_expr TIMES simple_expr { EMul ($1,$3) }
| factor_expr DIV   simple_expr { EDiv ($1,$3) }
| simple_expr                   { $1 }
;

simple_expr:
| INT               { EConst (VInt $1) }
| ID                { EVar   (Name $1) }
| TRUE              { mytrue }
| FALSE             { myfalse } 
| LPAR expr RPAR    { $2 }
| MINUS simple_expr { ESub (EConst (VInt 0), $2) }
;






