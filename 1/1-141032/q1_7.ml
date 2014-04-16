let reverse xs =
  let rec reverse_rec xs acc =
    match xs with
    | [] -> acc
    | y::ys -> reverse_rec ys (y::acc)
  in
  reverse_rec xs [];;
