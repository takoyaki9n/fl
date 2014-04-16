let appendr xs ys =
  List.fold_right (fun a b -> a::b) xs ys;;
  
let appendl xs ys =
  List.fold_left (fun a b -> a@[b]) xs ys;;
  
let lastr xs =
  List.fold_right (fun a b -> 
	      match b with
	      | None -> Some a
	      | _ -> b) xs None;;
  
let lastl xs =
  match xs with
  | [] -> None
  | y::ys -> Some (List.fold_left (fun a b -> b) y ys);;
  
let rec mapr f xs =
  List.fold_right (fun a b -> (f a)::b) xs [];;
  
let rec mapl f xs =
  List.fold_left (fun a b -> a@[f b]) [] xs;;
  
