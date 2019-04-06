type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree


let rec t_size (input_tree:'a tree):int=
	match input_tree with
	|Leaf _->1
	|Fork(root,left,right)->1+t_size left+t_size right

let rec t_sum (input_tree:int tree):int=
	match input_tree with
	|Leaf x->x
	|Fork(root,left,right)->root+t_sum left+t_sum right

let rec t_charcount (input_tree:string tree):int=
	match input_tree with
	|Leaf x->String.length(x)
	|Fork(root,left,right)->String.length(root)+
				t_charcount left+
				t_charcount right

let rec t_concat (input_tree:string tree):string=
	match input_tree with
	|Leaf x->x
	|Fork(root,left,right)->root^t_concat left^t_concat right

(*part B*)
let rec t_opt_size (input_tree:'a option tree):int=
	match input_tree with
	|Leaf None->0
	|Leaf _->1
	|Fork(root,left,right)->1+t_opt_size left+t_opt_size right

let rec t_opt_sum (input_tree:int option tree):int=
	match input_tree with
	|Leaf Some x->x
	|Leaf None->0
	|Fork(None,left,right)->0+t_opt_sum left+t_opt_sum right
	|Fork(Some root,left,right)->root+t_opt_sum left+t_opt_sum right

let rec t_opt_charcount (input_tree:string option tree):int=
	match input_tree with
	|Leaf None->0
	|Leaf Some x->String.length(x)
	|Fork(None,left,right)->0+t_opt_charcount left+t_opt_charcount right
	|Fork(Some root,left,right)->String.length(root)+
					t_opt_charcount left+
					t_opt_charcount right

let rec t_opt_concat (input_tree:string option tree):string=
	match input_tree with
	|Leaf None->""
	|Leaf Some x->x
	|Fork(None,left,right)->""^t_opt_concat left^t_opt_concat right
	|Fork(Some root,left,right)->root^t_opt_concat left^t_opt_concat right

(*part C*)
let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b = 
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_size (input_tree:'a tree):int=
	tfold (fun x->1) (fun x y z->1+y+z) input_tree

let tf_sum (input_tree:int tree):int=
	tfold (fun x->x) (fun x y z->x+y+z) input_tree

let tf_charcount(input_tree:string tree):int=
	tfold (fun x->String.length(x)) 
	(fun x y z->String.length(x)+y+z) 
	input_tree

let tf_concat (input_tree:string tree):string=
	tfold (fun x->x) (fun x y z->x^y^z) input_tree

let tf_opt_size(input_tree:'a option tree):int=
	let l =function
	|None->0
	|Some x->1
	in tfold l (fun x y z->l x+y+z) input_tree

let tf_opt_sum (input_tree:int option tree):int=
	let l=function
	|None->0
	|Some x->x
	in tfold l (fun x y z->l x+y+z) input_tree

let tf_opt_charcount (input_tree:string option tree):int=
	let l=function
	|None->0
	|Some x->String.length(x)
	in tfold l (fun x y z->l x+y+z) input_tree
	
let tf_opt_concat (input_tree:string option tree):string=
	let l=function
	|None->""
	|Some x->x
	in tfold l (fun x y z->l x^y^z) input_tree

(*part D*)
type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree


let rec bt_insert_by (compare:'a->'a->int) (elem:'a) (input_tree:'a btree):'a btree=
	match input_tree with
	|Empty->Node(Empty,elem,Empty)
	|Node(left,root,right)->if compare elem root<=0 
			        then Node(bt_insert_by compare elem left,root,right)
				else Node(left,root,bt_insert_by compare elem right)

let rec bt_elem_by (f:'a->'b->bool) (elem:'b) (input_tree:'a btree):bool=
	match input_tree with
	|Empty->false
	|Node(left,root,right)->if f root elem=true 
				then true
				else 
				bt_elem_by f elem left||bt_elem_by f elem right

let rec bt_to_list (input_tree:'a btree):'a list=
	match input_tree with
	|Empty->[]
	|Node(left,root,right)->bt_to_list left@root::bt_to_list right

let rec btfold (base:'b) (f:'b->'a->'b->'b) (input_tree:'a btree):'b=
	match input_tree with
	|Empty->base
	|Node(left,root,right)->f (btfold base f left) root (btfold base f right)
	
let btf_elem_by (f:'a->'b->bool) (elem:'b) (input_tree:'a btree):bool=
	btfold false (fun r1 root r2->f root elem||r1||r2) input_tree

let btf_to_list (input_tree:'a btree):'a list=
	btfold [] (fun l1 root l2->l1@root::l2) input_tree

(*btf_insert_by is difficult to write because if written in btfold form then the type
  of btfold base should be 'a btree and the type of f in btfold should be 
  (f:'a btree->'a->'a btree->'a btree).However, we still need a place to put the compare function
  in the tfold function,which is difficult to achieve.The result is a 'a btree，every step we need
  to build a tree and it is hard to do that in a fold function*)					
	

