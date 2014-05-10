module type EQ =
  sig
    type ('a, 'b) equal
    val refl: ('a, 'a) equal
    (* val symm: ('a, 'b) equal -> ('b, 'a) equal *)
    (* val trans: ('a, 'b) equal -> ('b, 'c) equal -> ('a, 'c) equal *)
    val apply: ('a, 'b) equal -> 'a -> 'b
  end

module Eq: EQ = 
struct
  type ('a, 'b) equal = ('a -> 'b) * ('b -> 'a)
  let refl = (fun a -> a, fun a -> a)
  (* let symm f = (snd f, fst f) *)
  (* let trans f g =  *)
  (*   match f, g with *)
  (*   | (fl, fr), (gl, gr) -> (fun a -> gl (fl a), fun c -> fr (gr c)) *)
  let apply f a = (fst f) a
  (* module Lift =  *)
  (*   functor(F: sig type 'a t end) -> *)
  (*   sig *)
  (*     val f =  *)
  (*   end *)
end
