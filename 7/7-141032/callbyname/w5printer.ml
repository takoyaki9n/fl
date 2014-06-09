open Format 
open Syntax

let pp_name fmt = function 
  | Name n -> fprintf fmt "%s" n

let rec pp_value fmt v = 
  match v with
  | VInt  i -> fprintf fmt "%d" i 
  | VBool b -> fprintf fmt "%B" b
  | VFun _ -> fprintf fmt "<fun>"
  | VRFun _ -> fprintf fmt "<fun>"
  | VNil | VCons (_, _) -> pp_vlist fmt v
  | VTup l -> pp_vtup fmt l

and pp_vlist fmt v = 
  let rec pp_vlist' fmt = function
    | VNil -> fprintf fmt "" 
    | VCons (x, VNil) -> fprintf fmt "%a" pp_value x
    | VCons (x, y) -> fprintf fmt "%a;@ %a" pp_value x pp_vlist' y in
  fprintf fmt "[@[<hov 2>%a@]]" pp_vlist' v

and pp_vtup fmt xs = 
  let rec pp_vtup' fmt = function
    | []    -> fprintf fmt "" 
    | [x]   -> fprintf fmt "%a" pp_value x 
    | x::xs -> fprintf fmt "%a,@ %a" pp_value x pp_vtup' xs in
  fprintf fmt "(@[<hov 2>%a@])" pp_vtup' xs

let rec pp_type fmt = function
  | TInt  -> fprintf fmt "int" 
  | TBool  -> fprintf fmt "bool" 
  | TVar v -> fprintf fmt "t%d" v
  | TFun (t1, t2) -> fprintf fmt "(%a -> %a)" pp_type t1 pp_type t2
  | TTup l -> pp_ttup fmt l
  | TList t -> fprintf fmt "%a list" pp_type t

and pp_ttup fmt xs = 
  let rec pp_ttup' fmt = function
    | []    -> fprintf fmt "" 
    | [x]   -> fprintf fmt "%a" pp_type x 
    | x::xs -> fprintf fmt "%a * %a" pp_type x pp_ttup' xs in
  fprintf fmt "(@[<hov 2>%a@])" pp_ttup' xs

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

let print_result n v t =
  match n with
  | None -> fprintf std_formatter "- : %a = %a@." pp_type t pp_value v
  | Some m -> fprintf std_formatter "val %a : %a = %a@." pp_name m pp_type t pp_value v;;
  
