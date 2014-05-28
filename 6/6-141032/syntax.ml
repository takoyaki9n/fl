open Format 

type name = Name of string 

type value =
  | VInt  of int
  | VBool of bool 
  | VFun  of name * expr * env
  | VRFun of name * expr * (env ref)
  | VList of value list
  | VTup  of value list
 
and  pat =
  | PConst of value 
  | PVar   of name
  | PCons  of pat * pat 
  | PNil 
  | PTup  of pat list

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
  | ETup   of expr list

and env = (name * value) list

type command =
  | CLet   of name * expr 
  | CRLets of (name * name * expr) list 
  | CExp of expr 
  | CQuit 

let pp_name fmt = function 
  | Name n -> fprintf fmt "%s" n

let rec pp_value fmt = function 
  | VInt  i -> fprintf fmt "%d" i 
  | VBool b -> fprintf fmt "%B" b
  | VFun _ -> fprintf fmt "<fun>"
  | VRFun _ -> fprintf fmt "<fun>"
  | VList l -> pp_vlist fmt l
  | VTup l -> pp_vtup fmt l

and pp_vlist fmt xs = 
  let rec pp_vlist' fmt = function
    | []    -> fprintf fmt "" 
    | [x]   -> fprintf fmt "%a" pp_value x 
    | x::xs -> fprintf fmt "%a;@ %a" pp_value x pp_vlist' xs in
  fprintf fmt "[@[<hov 2>%a@]]" pp_vlist' xs

and pp_vtup fmt xs = 
  let rec pp_vtup' fmt = function
    | []    -> fprintf fmt "" 
    | [x]   -> fprintf fmt "%a" pp_value x 
    | x::xs -> fprintf fmt "%a,@ %a" pp_value x pp_vtup' xs in
  fprintf fmt "(@[<hov 2>%a@])" pp_vtup' xs

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
  | PTup l -> 
    pp_ptup fmt l

and pp_ptup fmt xs = 
  let rec pp_ptup' fmt = function
    | []    -> fprintf fmt "" 
    | [x]   -> fprintf fmt "%a" pp_pat x 
    | x::xs -> fprintf fmt "%a,@ %a" pp_pat x pp_ptup' xs in
  fprintf fmt "(@[<hov 2>%a@])" pp_ptup' xs

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
     pp_bin fmt "ERLets" (pp_list pp_letrec) ds pp_expr e 
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
  | ETup l ->
     fprintf fmt "ETup(%a)" (pp_list pp_expr) l      

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

let print_result n v=
  let t = (match v with
	   | VInt i -> "int"
	   | VBool b -> "bool"
	   | VFun _ | VRFun _ -> "'a -> 'b"
	   | VList l -> "'a list"
	   | VTup l -> "'a * 'b") in
  match n with
  | None -> fprintf std_formatter "- : %s = %a@." t pp_value v
  | Some m -> fprintf std_formatter "val %a : %s = %a@." pp_name m t pp_value v;;
  
