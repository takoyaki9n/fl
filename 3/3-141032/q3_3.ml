#use "orderd_type.ml";;

module OrderSet (T : ORDERED_TYPE) : 
sig
  type t
  val empty : t
  val add : T.t -> t -> t
  val remove : T.t -> t -> t
  val mem : T.t -> t -> bool
  val size : t -> int
  val show : t -> T.t list
  val load : T.t list -> t -> t
end = 
  struct
    type t = Leaf | Node of T.t * t * t					
					
    let empty = Leaf
		  
    let rec add v tree =
      match tree with
      | Leaf -> Node (v, Leaf, Leaf)
      | Node (u, tl, tr) ->
	 match T.compare v u with
	 | EQ -> tree
	 | LT -> Node (u, add v tl, tr)
	 | GT -> Node (u, tl, add v tr)

    let rec remove_min tree =
      match tree with
      | Leaf -> (None, Leaf)
      | Node (v, Leaf, tr) -> (Some v, tr)
      | Node (v, tl, tr) -> 
	 let (u, t) = remove_min tl
	 in
	 (u, Node (v, t, tr))

    let rec remove v tree =
      match tree with
      | Leaf -> Leaf
      | Node (u, tl, tr) ->
	 match T.compare v u with
	 | EQ ->
    	    if tl = Leaf then tr
    	    else
	      (match (remove_min tr) with
	       | (None, _) -> tl
	       | (Some w, t) -> Node (w, tl, t))
	 | LT -> Node (u, remove v tl, tr)
	 | GT -> Node (u, tl, remove v tr)
    let rec mem v tree =
      match tree with
      | Leaf -> false
      | Node (u, tl, tr) ->
	 match T.compare v u with
	 | EQ -> true
	 | LT -> mem v tl
	 | GT -> mem v tr 

    let rec size tree =
      match tree with
      | Leaf -> 0
      | Node (u, tl, tr) -> 1 + (size tl) + (size tr)

    let show tree =
      let rec show_aux tree xs =
	match tree with 
	| Leaf -> xs
	| Node (v, tl, tr) -> show_aux tr (v::(show_aux tl xs))
      in
      List.rev (show_aux tree [])

    let load xs tree =
      List.fold_left (fun t x -> add x t) tree xs
  end
