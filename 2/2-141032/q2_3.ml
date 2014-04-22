type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;

let preorder t =
  let rec preorder_rec t xs =
    match t with 
    | Leaf -> xs
    | Node (v, tl, tr) -> preorder_rec tr (preorder_rec tl (v::xs))
  in
  List.rev (preorder_rec t []);;

let inorder t =
  let rec inorder_rec t xs =
    match t with 
    | Leaf -> xs
    | Node (v, tl, tr) -> inorder_rec tr (v::(inorder_rec tl xs))
  in
  List.rev (inorder_rec t []);;

let postorder t =
  let rec postorder_rec t xs =
    match t with 
    | Leaf -> xs
    | Node (v, tl, tr) -> v::(postorder_rec tr (postorder_rec tl xs))
  in
  List.rev (postorder_rec t []);;
