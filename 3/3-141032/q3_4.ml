#use "orderd_type.ml";;
   
module Dict (K : ORDERED_TYPE) :
sig
  type 'a t
  val empty : 'a t
  val add : K.t -> 'a -> 'a t -> 'a t
  val remove : K.t -> 'a t -> 'a t
  val lookup : K.t -> 'a t -> 'a option
  val size : 'a t -> int
  val show : 'a t -> (K.t * 'a) list
  val load : (K.t * 'a) list -> 'a t -> 'a t
end = 
  struct
    type 'a t = Leaf | Node of K.t * 'a * 'a t * 'a t
						    
    let empty = Leaf
		  
    let rec add k v tree =
      match tree with
      | Leaf -> Node (k, v, Leaf, Leaf)
      | Node (l, u, tl, tr) ->
    	 match K.compare k l with
    	 | EQ -> Node (l, v, tl, tr)
    	 | LT -> Node (l, u, add k v tl, tr)
    	 | GT -> Node (l, u, tl, add k v tr)

    let rec remove_min tree =
      match tree with
      | Leaf -> (None, None, Leaf)
      | Node (k, v, Leaf, tr) -> (Some k, Some v, tr)
      | Node (k, v, tl, tr) ->
    	 let (l, u, t) = remove_min tl
    	 in
    	 (l, u, Node (k, v, t, tr))

    let rec remove k tree =
      match tree with
      | Leaf -> Leaf
      | Node (l, u, tl, tr) ->
    	 match K.compare k l with
    	 | EQ ->
    	    if tl = Leaf then tr
    	    else
    	      (match (remove_min tr) with
    	       | (None, _, _) -> tl
    	       | (Some m, Some w, t) -> Node (m, w, tl, t))
    	 | LT -> Node (l, u, remove k tl, tr)
    	 | GT -> Node (l, u, tl, remove k tr)

    let rec lookup k tree =
      match tree with
      | Leaf -> None
      | Node (l, u, tl, tr) ->
    	 match K.compare k l with
    	 | EQ -> Some u
    	 | LT -> lookup k tl
    	 | GT -> lookup k tr

    let rec size tree =
      match tree with
      | Leaf -> 0
      | Node (k, u, tl, tr) -> 1 + (size tl) + (size tr)

    let show tree =
      let rec show_aux tree ps =
  	match tree with
  	| Leaf -> ps
  	| Node (k, v, tl, tr) -> show_aux tr ((k, v)::(show_aux tl ps))
      in
      List.rev (show_aux tree [])
	       
    let load ps tree =
      List.fold_left (fun t p -> add (fst p) (snd p) t) tree ps
  end
