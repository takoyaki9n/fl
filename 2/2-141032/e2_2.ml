type false_t = {any: 'a. 'a};;
type 'a not_t = 'a -> false_t;;
type ('a, 'b) and_t = 'a * 'b;;
type ('a, 'b) or_t = L of 'a | R of 'b;;

let compose =
  fun f g -> (fun x -> g (f x));;

let (distribution : ('a, ('b, 'c) and_t) or_t -> (('a, 'b) or_t, ('a, 'c) or_t) and_t) =
  fun p -> 
  match p with
  | L a -> (L a, L a)
  | R (b, c) -> (R b, R c);;

let (rev_distribution: (('a, 'b) or_t, ('a, 'c) or_t) and_t -> ('a, ('b, 'c) and_t) or_t) = 
  fun p ->
  match p with
  | (L a, _) | (_, L a) -> L a
  | (R b, R c) -> R (b ,c);;

let (excluded_middle1: ('a, 'a not_t) or_t not_t not_t) =
  fun q ->
  q (R (fun r ->
	q (L r)));;
  
let (excluded_middle2: ('a -> 'b) -> ('a not_t -> 'b) -> 'b not_t not_t) =
  fun q r s->
  s (r (fun x -> 
	s (q x)));;
  
let rec (contradiction: ('a -> 'a not_t -> 'c) -> 'c) =
  fun x -> contradiction x;;

let (pierce: (('a -> 'b) -> 'a) -> 'a not_t not_t) = 
  let rec (absurdity: false_t -> 'a) =
    fun x -> absurdity x
  in
  fun q r -> 
  (fun s -> r (q s)) (fun t -> absurdity (r t));;

(* let (p6: (('a -> 'b) -> 'a) -> 'a not_t not_t) =  *)
(*   fun (q: ('a -> 'b) -> 'a) (r: 'a not_t) ->  *)
(*   (fun (s: 'a -> 'b) -> r (q s)) (fun (t: 'a) -> pi (r t));; *)
    
