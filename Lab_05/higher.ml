(*Group member:Yuanhao Ruan,Benjamin Osborn*)
(*return even elements from the list*)
let all_evens =
	let even x=x mod 2=0 
in List.filter even

(*increment 1 to every element in the list*)
let increment_all=
	let inc x=x+1
in List.map inc

(*return the largest number in a int list*)
let max_fold (lst:int list) =
	match lst with
	|[]->raise (Failure "Input list must not be empty")
	|x::xs->let compare a b=if a>b then a else b in List.fold_right compare xs x

(*folds up a list on integers into a pair containing the sum and products.*)
let sum_prod lst =
	match lst with
	|[]->(0,1)
	|x->(List.fold_left (+) 0 x, List.fold_left ( * ) 1 x)
	
(*chops up a list into sub-lists using the function argument
let split f lst=
	let divide x (all_group,current_group)=
		if f x then (current_group::all_group,[]) else (all_group, x::current_group)  
		in let (groups,last)=List.fold_right divide lst ([],[])
		in (last::groups)*)

(*chops up a list into sublists using the function argument*)
let split (f:'a->bool) (lst:'a list):'a list list=
	let divide x (all_group,current_group)=
		if f x then (current_group::all_group,[]) 
		       else (all_group, x::current_group)  
		       in let (groups,last)=List.fold_right divide lst ([],[])
                       in (last::groups)
	
