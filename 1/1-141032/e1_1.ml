let reverser xs =
  let t = (List.fold_right (fun a b -> 
			    match b with 
			    | ([], ys) -> ([], ys)
			    | (y::ys, zs) -> (ys, y::zs)) xs (xs , []))
  in
  match t with
    (a, b) -> b;;
