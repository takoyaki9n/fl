let rec fold_right f xs e =
  match xs with
  | [] -> e
  | y::ys -> f y (fold_right f ys e);;
  
let rec fold_left f e xs =
  match xs with
  | [] -> e
  | y::ys -> fold_left f (f e y) ys;;
