type name = Name of string 

type value =
  | VInt  of int
  | VBool of bool
  | VFun  of name * expr * env
  | VCons of (content ref) * (content ref)
  | VNil
  | VTup  of (content ref) list
and pat =
  | PConst of value 
  | PVar   of name
  | PCons  of pat * pat 
  | PNil 
  | PTup  of pat list
and expr =
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
  | ERLets of (name * expr) list * expr 
  | EMatch of expr * (pat * expr) list 
  | EFun   of name * expr 
  | EApp   of expr * expr 
  | ECons  of expr * expr 
  | ENil   
  | ETup   of expr list

and thunk = expr * (env ref)

and content = 
  | CntVal   of value
  | CntThunk of thunk
		  
and env = (name * content ref) list
  
let empty_env: env = [];;

type tvar = int

type ty = 
  | TInt
  | TBool
  | TFun of ty * ty
  | TList of ty
  | TTup of ty list
  | TVar of tvar
	      
type ty_map = tvar * ty

type ty_env =  (name * ty) list

let empty_ty_env: ty_env = [];;

type command =
  | CLet   of name * expr 
  | CRLets of (name * expr) list 
  | CExp of expr 
  | CQuit 
