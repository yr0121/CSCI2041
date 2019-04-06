open Ordered


module type RedBlackSetSig =sig
	type elem
	type color = R | B
	type t = E | T of color * t * elem * t
	val empty:t
	val insert:elem->t->t
	val member:elem->t->bool
	val isRedBlackTree: t -> bool
end

module RedBlackTree (O:OrderedSig):
	(RedBlackSetSig with type elem=O.t)=struct
	type elem=O.t
	type color =R|B
	type t =E|T of color*t*O.t*t
	let empty=E
	let balance c t1 v t2=
	 	match c,t1,v,t2 with
	 	|B,T(R,T(R,a,x,b),y,c),z,d
	 	|B,T(R,a,x,T(R,b,y,c)),z,d
	 	|B,a,x,T(R,T(R,b,y,c),z,d)
	 	|B,a,x,T(R,b,y,T(R,c,z,d))->
	 	T(R,T(B,a,x,b),y,T(B,c,z,d))
	 	|a,b,c,d->T(a,b,c,d)

	let insert x s=
		let rec ins t=
		match t with
		| E -> T(R,E,x,E)
		| T(color,a,y,b) as s->
			if x<y
			then balance color (ins a) y b
			else
			if x>y
			then balance color a y (ins b)
			else s
		  in match ins s with
		  | T(_,a,y,b) -> T(B,a,y,b)
		  | E-> raise (Failure "Not work")
	 

	 let rec member e t=
	 	match e,t with
	 	|x,E->false
	 	|x,T(_,a,y,b)->
	 		if x<y 
	 		then member x a
	 		else if x>y
	 		then member x b
	 		else true


	 (* Invariant of red black tree: 
	 1.Every node is colored with black or red.The root is black.
	 2.A red node cannot have a parent whose color is also red. 
	 3.Every path from the node to an leaf node has the same number of black nodes
	 4.The tree also need to satisfy binary search tree properties:
	 for any node all the node in the left subtree should have smaller value than 
	 this node and all the right subtree should have larger value than this node *)

	 (* check if every node has right color *)
	 let test_color t:bool=match t with
	 	|E->true
	 	|T(B,_,_,_)->true
	 	|T(R,_,_,_)->true
	 
	 (* check root color *)
	 let root_color t:bool=match t with
	 	|E->true
	 	|T(B,_,_,_)->true
	 	|_->false

	 (* check invariant 2 *)
	 let rec check_parent t:bool=
	 	match t with
	 	|E->true
	 	|T(R,(T(R,_,_,_)),_,_)->false
	 	|T(R,_,_,(T(R,_,_,_)))->false
	 	|T(_,t1,_,t2)->check_parent t1&&check_parent t2

	 (* flatten the tree to a list *)
	 let rec inorder t:elem list=
	 	match t with
	 	|E->[]
	 	|T(_,t1,root,t2)->inorder t1@[root]@inorder t2

	 (* check if the flattened list has ascending order  *)
	 let rec check_list lst:bool=
	 	match lst with
	 	|[]->true
	 	|_::[]->true
	 	|x::y::tl->if O.leq x y
	 			   then check_list (y::tl)
	 			   else false

	 (* check if a tree is binary search tree *)
	 let rec check_BST t:bool=check_list(inorder t)

(* Extra credit:This method checks without explicitly generating all paths.
First, we should match the input red black tree. If the matched result is empty, simply
return 1. If the Tree has black root with some left subtree and right subtree,we can recursively
check the subtrees to determine if they have same number of black nodes. If they are not equal
or produced some reuslt of -1, we know that they have different number of black nodes and just
return -1 to indicate that, or we simply return the number of black nodes+1 to reflect the final
number of black nodes because the root is also black.If we have a tree of red node, the same
principal applies except we only return the number of subtrees and do not add 1 since the root
is a red node. By checking if the result greater than -1 or not, we can determine if all paths 
from the root to a leaf node have the same number of black nodes *)
	let rec check_black_node t:int=
		match t with
		|E->1
		|T(B,t1,_,t2)->
			if check_black_node t1 <>check_black_node t2
			||check_black_node t1= -1 
			||check_black_node t2= -1
			then -1
			else 1+check_black_node t1
		|T(R,t1,_,t2)->
			if check_black_node t1 <>check_black_node t2
			||check_black_node t1= -1 
			||check_black_node t2= -1
			then -1
			else check_black_node t1

	(* check if red black tree *)
	let isRedBlackTree t:bool=
		test_color t&&root_color t&&check_BST t&&check_parent t
		&&check_black_node t> -1

end


module RBTI = RedBlackTree (Int)
let()=
		assert(RBTI.insert 2 E=T(B,E,2,E));
		assert(RBTI.insert 10 (T(B,E,2,E))=T(B,E,2,T(R,E,10,E)));
		assert(RBTI.member 3 (T(B,T(R,E,2,E),3,T(R,E,10,E)))=true);
		assert(RBTI.member 1 (T(B,T(R,E,1,E),2,T(R,E,10,E)))=true);
		assert(RBTI.member 5 (T(B,T(R,E,1,E),2,T(R,E,10,E)))=false);
		print_endline("Success,all tests passed")



