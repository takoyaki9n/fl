open Format 

type name = Name of string 

type value =
  | VInt  of int
  | VBool of bool 
  (* 拡張の必要あり *) 

and  pat =
  | PConst of value 
  | PVar   of name
  | PCons  of pat * pat 
  | PNil 

and  expr =
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
  | ERLets of (name * name * expr) list * expr 
  | EMatch of expr * (pat * expr) list 
  | EFun   of name * expr 
  | EApp   of expr * expr 
  | ECons  of expr * expr 
  | ENil   

type command =
  | CLet   of name * expr 
  | CRLets of (name * name * expr) list 
  | CExp of expr 
  | CQuit 

let pp_name fmt = function 
  | Name n -> fprintf fmt "%s" n

let pp_value fmt = function 
  | VInt  i -> fprintf fmt "%d" i 
  | VBool b -> fprintf fmt "%B" b

let pp_list pp fmt xs = 
  let rec pp_list' fmt = function
    | []    -> fprintf fmt "" 
    | [x]   -> fprintf fmt "%a" pp x 
    | x::xs -> fprintf fmt "%a,@ %a" pp x pp_list' xs in
  fprintf fmt "[@[<hov 2>%a@]]" pp_list' xs
    

let pp_bin fmt s f1 e1 f2 e2 =
  fprintf fmt "%s (@[<hov 2>%a,@ %a@])" s f1 e1 f2 e2 

let rec pp_pat fmt = function 
  | PConst v -> 
    pp_value fmt v 
  | PVar x -> 
    pp_name fmt x
  | PCons (p1,p2) ->
    pp_bin fmt "PCons" pp_pat p1 pp_pat p2 
  | PNil -> 
    fprintf fmt "PNil" 

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
    fprintf fmt "EIf (@[<hov 2>%a,@ %a,@ %a@])"
      pp_expr e1 pp_expr e2 pp_expr e3 
  | ELet (n,e1,e2) -> 
    fprintf fmt "ELet (@[<hov 2>%a,@ %a,@ %a@])"
      pp_name n pp_expr e1 pp_expr e2
  | ERLets (ds,e) -> 
    pp_bin fmt "ELets" (pp_list pp_letrec) ds pp_expr e 
  | EMatch (e,alts) -> 
    pp_bin fmt "EMatch" pp_expr e (pp_list pp_alt) alts 
  | EFun (n,e) -> 
    pp_bin fmt "EFun" pp_name n pp_expr e 
  | EApp (e1,e2) -> 
    pp_bin fmt "EApp" pp_expr e1 pp_expr e2 
  | ECons (e1,e2) -> 
    pp_bin fmt "ECons" pp_expr e1 pp_expr e2 
  | ENil -> 
    fprintf fmt "ENil"

and pp_alt fmt (p,e) =
  fprintf fmt "<@[<hov 2>%a,@,%a@]>" pp_pat p pp_expr e

and pp_letrec fmt (f,n,e) =
  fprintf fmt "<@[<hov 2>%a,@,%a,@,%a@]>" pp_name f pp_name n pp_expr e 

let rec pp_command fmt = function 
  | CLet (n,e) ->
    pp_bin fmt "CLet" pp_name n pp_expr e 
  | CRLets ds -> 
    fprintf fmt "CRLets %a" (pp_list pp_letrec) ds 
  | CExp e -> 
    fprintf fmt "CExp (%a)" pp_expr e 
  | CQuit -> 
    fprintf fmt "\nCQuit"

let print_expr expr = 
  fprintf std_formatter "%a@." pp_expr expr 

let print_command command = 
  fprintf std_formatter "%a@." pp_command command
