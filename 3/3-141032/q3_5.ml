module type RING =
  sig 
    type t
    val zero : t
    val one : t
    val add : t -> t -> t
    val mul : t -> t -> t
  end

module type MATRIX = 
  functor (R: RING) ->
	  sig
	    type elm = R.t
	    type mat
	    val elem : int -> int -> mat -> elm
	    val trans : mat -> mat
	    val row : int -> mat -> elm list
	    val col : int -> mat -> elm list
	    val add : mat -> mat -> mat
	    val scale : elm -> mat -> mat
	    val mul : mat -> mat -> mat
	    val mat2list :  mat -> elm list list
	    val list2mat : elm list list -> mat
	  end

module Matrix: MATRIX =
  functor (R: RING) ->
	  struct
	    type elm = R.t
	    type mat = R.t list list
			   
	    let rec elem i j a =
	      match a with
	      | [] | []::_ -> failwith "invalid index"
	      | (x::v)::a ->
		 if i == 0 then
		   if j == 0 then
		     x
		   else
		     elem i (j - 1) (v::a)
		 else
		   elem (i - 1) j a
			
	    let rec trans a =
	      match a with
	      | [] -> []
	      | [v] -> List.map (fun x -> [x]) v
	      | v::a -> List.map2 (fun x v -> x::v) v (trans a)
				  
	    let rec row i a =
	      match a with
	      | [] -> failwith "invalid index"
	      | v::a -> 
		 if i == 0 then
		   v
		 else
		   row (i - 1) a
		       
	    let col i a =
	      row i (trans a)
		  
	    let add a b =
	      try 
		List.map2 
		  (fun u v -> 
		   List.map2 
		     (fun x y -> R.add x y) u v) a b 
	      with
	      | Invalid_argument m -> failwith "unmatch size"

	    let scale x a =
	      List.map 
		(fun v -> 
		 List.map 
		   (fun y -> R.mul x y) v) a

	    let mul a b =
	      try 
		List.map 
		  (fun u -> 
		   List.map 
		     (fun v -> 
		      let z = R.mul (List.hd v) (List.hd u) in
		      List.fold_left2 
			(fun z x y ->
			 R.add z (R.mul x y)) z (List.tl v) (List.tl u)) a) (trans b)
	      with
	      | Invalid_argument m -> failwith "unmatch size"				       

	    let mat2list a = a

	    let list2mat a = a
	  end

module type VECTOR = 
  functor (R: RING) ->
	  sig
	    type elm = R.t
	    type vec
	    val elem : int -> vec -> elm
	    val add : vec -> vec -> vec
	    val scale : elm -> vec -> vec
	    val matmul : Matrix(R).mat -> vec -> vec
	    val vec2list : vec -> elm list
	    val list2vec : elm list -> vec
	  end
	    
module Vector: VECTOR = 
  functor (R: RING) ->
	  struct
	    module M = Matrix(R)
	    type elm = R.t
	    type vec = M.mat
	    let elem i v = M.elem i 0 v
	    let add u v = M.add u v
	    let scale a v = M.scale a v
	    let matmul a v = M.mul a v
	    let vec2list v = M.col 0 v
	    let list2vec v = M.trans (M.list2mat [v])
	  end
