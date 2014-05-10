open Format

type name = string;;

type value =
  | VInt  of int;;

type expr =
  | EConst of value 
  | EVar   of name 
  | EAdd   of expr * expr 
  | ELet   of name * expr * expr;;

let string_of_name name = name;;

let string_of_value = function 
  | VInt i -> string_of_int i;;

let rec string_of_expr = function
  | EConst v -> 
     string_of_value v 
  | EVar x -> 
     string_of_name x
  | EAdd (e1,e2) -> 
     sprintf "EAdd (%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | ELet (n,e1,e2) -> 
     sprintf "ELet (%s, %s, %s)" (string_of_name n) (string_of_expr e1) (string_of_expr e2);;
    
let print_expr expr = print_string (string_of_expr expr);;


 ../../Week4/example/Makefile  ../../Week4/example/OCamlMakefile  ../../Week4/example/example.ml  ../../Week4/example/exampleLexer.mll  ../../Week4/example/exampleParser.mly  ../../Week4/example/syntax.ml
