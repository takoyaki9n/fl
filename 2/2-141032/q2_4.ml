type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;

(*
let t = Node (0, 
	      Node (1, 
		    Node (2, Leaf, Leaf), 
		    Node (3, 
			  Node (4, Leaf, Leaf), 
			  Node (5, Leaf, Leaf))), 
	      Node (6, Leaf, Leaf));;
  *)

let levelorder t = 
  let rec levelorder_rec q acc = 
    match q with
    | [] -> acc
    | t::p -> 
       match t with
       | Leaf -> levelorder_rec p acc
       | Node (v, tl, tr)-> levelorder_rec (p@[tl; tr]) (v::acc)
  in
  List.rev (levelorder_rec [t] []);;
