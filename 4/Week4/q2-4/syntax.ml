open Format 

type name = Name of string 

type value =
  | VInt  of int
  | VBool of bool 

type expr =
  | EConst of value 
  | EVar   of name 
  | EAdd   of expr * expr 
  | ESub   of expr * expr 
  | EMul   of expr * expr 
  | EDiv   of expr * expr 
  | ELT    of expr * expr 
  | EEq    of expr * expr 
  | EIf    of expr * expr * expr 
  | ELet   of name * expr * expr 

type command =
  | CLet of name * expr 
  | CExp of expr 
  | CQuit 

let pp_name fmt = function 
  | Name n -> fprintf fmt "%s" n

let pp_value fmt = function 
  | VInt  i -> fprintf fmt "%d" i 
  | VBool b -> fprintf fmt "%B" b

let pp_bin fmt s f1 e1 f2 e2 =
  fprintf fmt "%s (@[<hov 2>%a,@,%a@])" s f1 e1 f2 e2 

let rec pp_expr fmt = function 
  | EConst v -> 
    pp_value fmt v 
  | EVar x -> 
    pp_name fmt x 
  | EAdd (e1,e2) -> 
    pp_bin fmt "EAdd" pp_expr e1 pp_expr e2 
  | ESub (e1,e2) -> 
    pp_bin fmt "ESub" pp_expr e1 pp_expr e2
  | EMul (e1,e2) -> 
    pp_bin fmt "EMul" pp_expr e1 pp_expr e2 
  | EDiv (e1,e2) -> 
    pp_bin fmt "EDiv" pp_expr e1 pp_expr e2
  | ELT (e1,e2) -> 
    pp_bin fmt "ELT" pp_expr e1 pp_expr e2 
  | EEq (e1,e2) -> 
    pp_bin fmt "EEq" pp_expr e1 pp_expr e2 
  | EIf (e1,e2,e3) ->
    fprintf fmt "EIf (@[<hov 2>%a,@,%a,@,%a@])"
      pp_expr e1 pp_expr e2 pp_expr e3 
  | ELet (n,e1,e2) -> 
    fprintf fmt "ELet (@[<hov 2>%a,@,%a,@,%a@])"
      pp_name n pp_expr e1 pp_expr e2

let rec pp_command fmt = function 
  | CLet (n,e) ->
    pp_bin fmt "CLet" pp_name n pp_expr e 
  | CExp e -> 
    fprintf fmt "CExp (%a)" pp_expr e 
  | CQuit -> 
    fprintf fmt "\nCQuit"

let print_expr expr = 
  fprintf std_formatter "%a@." pp_expr expr 

let print_command command = 
  fprintf std_formatter "%a@." pp_command command
