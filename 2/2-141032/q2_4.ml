type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;

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
