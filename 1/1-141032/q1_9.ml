let rec pick_one_list xs =
  match xs with
  | [] -> []
  | y::ys ->
     let ts = pick_one_list ys
     in
     (y, ys)::(List.map (fun t -> 
			 match t with
			   (z, zs) -> (z, y::zs)) ts);;
  
let rec perm xs =
  match xs with
  | [] -> []
  | [y] -> [[y]]
  | ys -> 
     let ts = pick_one_list ys
     in
     let xss = 
       (List.map (fun t -> 
		  match t with
		    (z, zs) -> 
		    (List.map (fun ws -> z::ws) (perm zs))) ts)
     in
     List.fold_right (fun a b -> a @ b) xss [];;
  
