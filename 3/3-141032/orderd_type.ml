type order = LT | EQ | GT;;

module type ORDERED_TYPE =
  sig 
    type t
    val compare : t -> t -> order 
  end

module OrderedInt =
  struct
    type t = int
    let compare x y =
      if x < y then LT
      else if x > y then GT
      else EQ
  end

module OrderedString = 
  struct
    type t = string
    let compare x y =
      if String.compare x y < 0 then LT
      else if String.compare x y > 0 then GT
      else EQ
  end
