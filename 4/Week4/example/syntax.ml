type name = string 

type value =
  | VInt  of int

type expr =
  | EConst of value 
  | EVar   of name 
  | EAdd   of expr * expr 
  | ELet   of name * expr * expr 

let print_name = print_string 

let print_value = function 
  | VInt i -> print_int i 

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
 を活用すること
*)
let rec print_expr = function 
  | EConst v -> 
    print_value v 
  | EVar x -> 
    print_name x
  | EAdd (e1,e2) -> 
    (print_string "EAdd (";
     print_expr e1;
     print_string ",";
     print_expr e2;
     print_string ")")
  | ELet (n,e1,e2) ->
    (print_string "ELet (";
     print_name   n;
     print_string ","; 
     print_expr   e1;
     print_string ",";
     print_expr   e2;
     print_string ")")
      
       


    
