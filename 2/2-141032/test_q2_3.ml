#use "q2_3.ml";;

let t = Node (0, 
	      Node (1, 
		    Node (2, Leaf, Leaf), 
		    Node (3, 
			  Node (4, Leaf, Leaf), 
			  Node (5, Leaf, Leaf))), 
	      Node (6, Leaf, Leaf));;

preorder t;;

inorder t;;

postorder t;;
