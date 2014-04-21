type church = {t: 'a. ( 'a->'a ) -> 'a -> 'a};;

exception Negative_number;;

let add c1 c2 =
  {t = fun f x -> c1.t f (c2.t f x)};;

let mul c1 c2 =
  {t = fun f x -> c1.t (c2.t f) x};;

let rec i2c i = 
    if i < 0 then
      raise Negative_number
    else
      if i = 0 then
	{t=  fun f x -> x}
      else
	let c = i2c (i - 1)
	in
	{t = fun f x -> f ((c.t f) x)};;

let c2i c =
  c.t (fun x -> x + 1) 0;;
