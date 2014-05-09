%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <string> ID 
%token LET IN EQ
%token PLUS 
%token LPAR RPAR 
%token EOF 

%start main 
%type <Syntax.expr> main
%% 

main: 
expr EOF {$1}
;

expr:
  | LET var EQ expr IN expr   { ELet($2,$4,$6) }
  | arith_expr                { $1 } 
;

arith_expr:
  | arith_expr PLUS factor_expr { EAdd($1,$3) }
  | factor_expr                 { $1 }
;

factor_expr: 
  | atomic_expr                 { $1 }
;

atomic_expr:
  | INT            { EConst (VInt $1) }
  | ID             { EVar   $1 }
  | LPAR expr RPAR { $2 }
;

var:
  | ID  { $1 } 
;
 

