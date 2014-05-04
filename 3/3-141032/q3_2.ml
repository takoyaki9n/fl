module Stack :
sig
  exception Empty
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a * 'a t
  val size : 'a t -> int
end = 
  struct
    exception Empty
    type 'a t = 'a list
    let empty = []
    let push x xs = x::xs
    let pop xs = 
      match xs with
      | [] -> raise Empty
      | y::ys -> (y, ys)
    let size xs =
      let rec size_aux xs n =
	match xs with
	| [] -> n
	| y::ys -> size_aux ys (n + 1)
      in
      size_aux xs 0
  end;;
  
