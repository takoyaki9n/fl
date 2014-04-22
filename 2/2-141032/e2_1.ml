type church = {t: 'a. ( 'a->'a ) -> 'a -> 'a};;

exception Negative_number;;

let zero = {t = fun f x -> x};;

let scc c =
  {t = fun f x -> f (c.t f x)};;

let pred c =
  {t = fun f x -> c.t (fun g h -> h (g f)) (fun u -> x) (fun u -> u)};;
  
let add c1 c2 =
  {t = fun f x -> c1.t f (c2.t f x)};;

let sub c1 c2 =
  {t = fun f x -> ((c2.t pred) c1).t f x};;

let mul c1 c2 =
  {t = fun f x -> c1.t (c2.t f) x};;

let pow c1 c2 =
  {t = fun f x -> (c2.t c1.t) f x};;

let rec i2c i = 
  if i < 0 then
    raise Negative_number
  else
    if i = 0 then
      zero
    else
      scc (i2c (i - 1));;

let c2i c =
  c.t (fun x -> x + 1) 0;;
