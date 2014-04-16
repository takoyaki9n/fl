let fold_rightl f xs e =
  let rev = List.fold_left (fun a b -> b::a) [] xs
  in
  List.fold_left (fun a b -> f b a) e rev;;
  
let fold_leftr f e xs =
  let t = List.fold_right (fun a b -> 
			   match b with
			   | ([], ys) -> ([], ys)
			   | (y::ys, zs) -> (ys, f zs y)) xs (xs, e)
  in
  match t with
    (a, b) -> b;;
  
