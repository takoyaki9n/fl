type value = VInt of int | VBool of bool;;

type expr = EConst of value
	  | EAdd of expr * expr
	  | ESub of expr * expr
	  | EMul of expr * expr
	  | EDiv of expr * expr
	  | EEq of expr * expr
	  | ELt of expr * expr
	  | EIf of expr * expr * expr;;
