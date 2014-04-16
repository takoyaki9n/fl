let twice f =
  (fun x -> f (f x));;

let repeat f n =
  let rec repeat_rec f g n =
    if n <= 1 then
      g
    else
      repeat_rec f (fun x -> f (g x)) (n - 1)
  in
  repeat_rec f f n;;
