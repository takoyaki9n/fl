let rec append xs ys =
  match xs with
  | [] -> ys
  | z::zs -> z::(append zs ys);;

let rec last xs =
  match xs with
  | [] -> None
  | y::[] -> Some y
  | y::ys -> last ys;;

let rec map f xs =
  match xs with
  | [] -> []
  | y::ys -> (f y)::(map f ys);;
