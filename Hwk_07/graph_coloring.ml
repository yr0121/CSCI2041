(* All the explanations below refers to the color_option function
1.The search space is node list.In the case pf graph coloring problem, I made a decision about
 which color I should use to color the current node.
In the color_option function, I used a helper function try_color to find the possible solution.
"try_color ((assign x (C 1))@color_list) xs","try_color ((assign x (C 2))@color_list) xs" and 
try_color ((assign x (C 3))@color_list) xs are the three possible color I can assign to the current node.

2.The solution explores the answers by coloring the nodes one by one. The solution would try to 
assign the color to the current node to meet the requirement. 
"try_color ((assign x (C 1))@color_list) xs","
try_color ((assign x (C 2))@color_list) xs" and 
"try_color ((assign x (C 3))@color_list) xs" can be used to avoid this inefficiency because if adjacent nodes
can be colored with different colors, these operations would definitely find the available options if there 
exsists one.
Once a node is assigned with a color same to its adjacent nodes,it would not pass the check_adj and finally
exit the check process and output None
*)


type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list

(* find the color of the node *)
let rec look_up (n:node) (clst:coloring):color option=
	match clst with
	|[]->None
	|(n',v)::xs->if n'=n 
				 then Some v 
				 else look_up n xs

(* check if adjacent nodes has different color,if yes, return true
otherwise return false *)
let rec check_adj (clst:coloring) (e:edge list):bool=
	match e with
	|[]->true
	|(n1,n2)::xs->match look_up n1 clst,look_up n2 clst with
				  |Some x1,Some x2->if x1=x2
				  					then false
				  					else check_adj clst xs
				  |_->false
	 

let ok_color (clst:coloring) (g:graph)=check_adj clst (snd g)


(* assign a color to a node *)
let assign (n:node) (c:color):coloring=[(n,c)]

 let color_option (g:graph):coloring option=
 	let rec try_color color_list rest=
 		if ok_color color_list g&&color_list<>[]&&rest=[]
 		then Some color_list
 		else 
 		match rest with
 		|[]->None
 		|x::xs->(match try_color ((assign x (C 1))@color_list) xs with
 				|Some result->Some result
 				|None->(match try_color ((assign x (C 2))@color_list) xs with
 					   |Some result->Some result
 					   |None-> try_color ((assign x (C 3))@color_list) xs 
 					   		)
 					)
 		in try_color [] (fst g)

exception FoundColoring of coloring

let color_exception (g:graph):unit=
 	let rec try_color color_list rest=
 		if ok_color color_list g&&color_list<>[]&&rest=[]
 		then raise (FoundColoring color_list)
 		else 
 		match rest with
 		|[]->()
 		|x::xs->try_color ((assign x (C 1))@color_list) xs;
 				try_color ((assign x (C 2))@color_list) xs;
 				try_color ((assign x (C 3))@color_list) xs
 		in try_color [] (fst g)