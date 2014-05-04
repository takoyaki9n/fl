module type RING =
  sig 
    type t
    val zero : t
    val one : t
    val add : t -> t -> t
    val mul : t -> t -> t
  end

module type MATRIX = 
  sig
    type elm
    type mat
    val elem : int -> int -> mat -> elm
    val trans : mat -> mat
    val add : mat -> mat -> mat
    val scale : elm -> mat -> mat
    val mul : mat -> mat -> mat
    val mat2list :  mat -> elm list list
    val list2mat : elm list list -> mat
  end

module Matrix(R: RING) =
  struct
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

module Vector (R: RING) = 
  struct
    module M = Matrix(R)

    let elem i v = M.elem i 0 v

    let scale a v = M.scale a v

    let matmul a v = M.mul a v

    let vec2list v = M.col 0 v

    let list2vec v = M.trans (M.list2mat [v])
  end
