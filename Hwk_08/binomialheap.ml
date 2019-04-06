open Ordered


module type BinomialHeapSig = sig
  type elem
  type tree = Node of int * elem * tree list
  type t = tree list 
  val empty:t
  val isempty:t->bool
  val insert:elem->t->t
  val merge:t->t->t
  val findMin:t->elem
  val deleteMin:t->t
  val findMinDirect:t->elem
  val isBinomialTree:tree->bool
  val isBinomialHeap:t->bool
end

module BinomialHeap(O:OrderedSig):
	(BinomialHeapSig with type elem=O.t)=struct
	  type elem = O.t
	  type tree = Node of int * O.t* tree list
  	type t = tree list
  	let empty=[]
  	let isempty ts=ts=[]
  	let rank(Node(r,x,c))=r
  	let root(Node(r,x,c))=x

  	let link (((Node(r,x1,c1)) as t1),
  			  ((Node(_,x2,c2)) as t2))=
  			   if O.leq x1 x2
  			   then Node(r+1,x1,t2::c1)
  			   else Node(r+1,x2,t1::c2)
  	
  	let rec insTree t1 t2=
  		match t1,t2 with
  		|t,[]->[t]
  		|t,((t'::ts') as ts)->
  						if rank t<rank t'
  						then t::ts
  						else insTree (link (t,t')) ts'
  	
  	let insert x ts=insTree (Node (0,x,[])) ts
  	
  	let rec merge tree1 tree2=
  		match tree1,tree2 with
  		|ts1,[]->ts1
  		|[],ts2->ts2
  		|((t1::ts1') as ts1),((t2::ts2') as ts2)->
  			if rank t1<rank t2
  			then t1::merge ts1' ts2
  			else if rank t2<rank t1
  			then t2::merge ts1 ts2'
  			else insTree (link(t1,t2)) (merge ts1' ts2')
  	
  	let rec removeMinTree trees=
  		match trees with
      |[]->raise(Failure "Empty tree")
  		|[t]->(t,[])
  		|t::ts->let (t',ts')=removeMinTree ts
  				in if O.leq (root t) (root t')
  				   then (t,ts)
  				   else (t',t::ts')
  	
  	let findMin ts=let (t,_)=removeMinTree ts
  				   in root t 
  	
  	let deleteMin ts=
  		let (Node(_,_,ts1),ts2)=removeMinTree ts
  		in merge (List.rev ts1) ts2

    let rec findMinDirect trees=
      match trees with
      |[]->raise (Failure "Empty")
      |[t]->root t
      |Node(_,x1,_)::(Node(_,x2,_)::xs as ys)->if O.leq x1 x2 
                                      then x1
                                      else findMinDirect ys




(* Invariants of binomial tree:
  1.Binomial tree of rank 0 should have only one node with an
  empty list.
  2.Binomial tree with rank r has 2^r nodes. 
  3.Binomial tree with rank r can also be regarded as a root node and list of childrens
  whose ranks are r-1,r-2...0.
  4.The list of children should have decreasing order of rank and
  elements should maintain heap order
*)
    
    (* Get the childre list of a binomial tree *)
    let get_children t:tree list=
      match t with
      |Node(_,_,c)->c

    (*Get the number of elements of a binomial tree  *)
    let number t:int=
      let rec helper lst=
      match lst with
      |[]->1
      |x::xs->1+helper xs
    in helper (get_children t)

    (* check if the nodes number id 2^rank *)
    let check_number t:bool=number t=int_of_float 
                            (2.**(float_of_int (rank t)))

    (* Return the rank of every Node as a list *)
    let take_rank t:int list=
        match t with
        |Node(r,_,c)->let children_rank=
                      let rec helper lst=
                      match lst with
                      |[]->[]
                      |x::xs->rank x::helper xs
                    in helper c
        in r::children_rank
    

    (* Check if a list is sorted in ascending order and every number is 1 larger than the previous number *)
    let rec sorted lst:bool=
      match lst with
      |[]->true
      |[x]->true
      |hd::sd::tl->if sd=hd-1
                   then sorted (sd::tl)
                   else false

    (* check invriant 3 *)
    let check_rank t:bool=
      match t with
      |Node(_,_,c)->let rec helper lst=
                    match lst with
                    |[]->true
                    |x::xs->sorted (take_rank t)&&helper xs
                  in helper c

    (* Get the element of every node and return as a list *)
    let take_elements t:elem list=
      match t with
      |Node(r,e,c)->let children_elem =
                    let rec helper lst=
                      match lst with
                      |[]->[]
                      |x::xs->root x::helper xs
                    in helper c
        in e::children_elem

    (* check id the elements are in descending order*)
    let rec check_elem lst:bool=
      match lst with
      |[]->raise (Failure "Not possible")
      |[x]->true
      |hd::sd::tl->if sd>=hd
                   then check_elem (sd::tl)
                   else false
    
    (* check elements are in heap order *)
    let check_heap t:bool=
      match t with
      |Node(_,_,c)->let rec helper lst=
                    match lst with
                    |[]->true
                    |x::xs->check_elem (take_elements t)&& helper xs 
                  in helper c
    
    (* check if the input tree is binomial tree *)
    let isBinomialTree t=check_number t&&check_rank t&&check_heap t
(* Invariants of binomial heap:
1.Every element in binimial heap is a binomial tree
2.No two trees in binomial heap has the same rank
3.The trees are stored in increasing order of rank
4.A binomial heap with n nodes has at most floor(1+logn) trees*)
    
    (* check if every tree in heap have different rank *)
    let rec diff_rank heap:bool=
      match heap with
      |[]->true
      |[x]->true
      |x::xs::tl->if x=xs
                  then false
                  else diff_rank (xs::tl)

    (* check if every tree in heap have increasing rank *)
    let rec inc_rank heap:bool=
      match heap with
      |[]->true
      |[x]->true
      |x::xs::tl->if rank x>rank xs
                  then false
                  else inc_rank (xs::tl)

    (* get the number of nodes in heap *)
    let rec get_number_of_nodes heap:int=
      match heap with
      |[]->0
      |x::xs->number x+get_number_of_nodes xs

    (* Sum up every nodes in heap *)
    let rec sum heap:float=
      match heap with
      |[]->0.0
      |x::xs->2.** (float_of_int (rank x))+.sum xs

    (* check invariant 4 *)
    let same heap:bool=get_number_of_nodes heap=int_of_float(sum heap)

    (* check if every element in the heap is a binomial tree *)
    let rec check_tree heap:bool=
      match heap with
      |[]->true
      |x::xs->if isBinomialTree x 
              then check_tree xs
              else false

    (* check if binomial heap *)
    let rec isBinomialHeap heap:bool=
    check_tree heap&&diff_rank heap&&inc_rank heap&&same heap
   
end

module BHI = BinomialHeap(Int)
	let h1 = BHI.empty
	let h2 = BHI.insert 20 h1
	let h3 = BHI.insert 30 h2
	let h4 = BHI.insert 10 h3
	let h5 = BHI.insert 40 h4

	let m1 = BHI.findMin h5

	let h6 = BHI.deleteMin h5

	let m2 = BHI.findMin h6

  let m3 = BHI.findMinDirect h6
