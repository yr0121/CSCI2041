(* Reference:compare_bintrees.ml *)

type 'a bintree
  = Lf of 'a
  | Nd of 'a bintree * 'a bintree

let t1 = Nd(Nd(Lf(1), Lf(2)), Nd(Lf(3), Lf(4)))
and t2 = Nd(Lf(1), Nd(Lf(2), Nd(Lf(3), Lf(4))))
and t3 = Nd(Nd(Nd(Lf(1), Lf(2)), Lf(3)), Lf(4))
and t4 = Nd(Lf(4), Nd(Lf(3), Nd(Lf(2), Lf(1))))
and t5 = Nd(Nd(Nd(Lf(4), Lf(3)), Lf(2)), Lf(1))

(* Trees t1, t2, and t3 have the same fringe. *)
(* Trees t4 and t5 have the same fringe. *)

type 'a lazee = 'a hidden ref
and 'a hidden
	= Value of 'a
	| Thunk of (unit -> 'a);;

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")


type 'a lazy_list
  = Cons of 'a * 'a lazy_list lazee
  | Nil

let rec append_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : 'a lazy_list =
   match l1 with
  | Nil->l2
  | Cons(hd,tl)->Cons(hd, delay(fun()->append_lazy (demand tl) l2))

let rec equal_list_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : bool =
  match l1,l2 with
  |Nil,Nil->true
  |Nil,Cons _->false
  |Cons _,Nil->false
  |Cons(hd1,tl1),Cons(hd2,tl2)->
            if hd1=hd2 
  					then equal_list_lazy (demand tl1) (demand tl2) 
  					else false

let rec flatten_lazy (t: 'a bintree) : 'a lazy_list =
  match t with
  |Lf leaf->Cons(leaf,delay (fun()->Nil))
  |Nd(t1,t2)->append_lazy (flatten_lazy t1) (flatten_lazy t2)

let eqleaves_lazy (t1: 'a bintree) (t2: 'a bintree) : bool =
	match t1,t2 with
	|Lf leaf1,Lf leaf2->leaf1=leaf2
	|Lf leaf,Nd _->false
	|Nd _,Lf leaf->false
	|Nd(Lf leaf1,t1'),Nd(Lf leaf2,t2')->
            if leaf1=leaf2 
						then 
						equal_list_lazy 
						(flatten_lazy t1') (flatten_lazy t2') 
						else false
	|Nd(t1',Lf leaf1),Nd(t2',Lf leaf2)->
            if leaf1=leaf2 
						then equal_list_lazy 
						(flatten_lazy t1') (flatten_lazy t2') 
						else false
	|_,_->equal_list_lazy (flatten_lazy t1) (flatten_lazy t2)

(* The lazy method uses less space because it only append and flatten trees when necessary.If evaluated to false,
then the lazy method would immediately stop the process and we do not need to append and flatten the remaining part 
of the tree.In equaleaves_lazy, we first check if t1 and t2 are only composed of Lf.If they are only composed of Lf,
we do not need to conver trees to lazy list.Then if one of t1 and t2 is Lf, and the other is Nd_,we do not need to 
flatten the tree and we know the result is false immediately.Then if t1 and t2 have stuctures of Nd(Lf,_),Nd(Lf,_) 
or Nd(_,Lf),Nd(_,Lf),we can compare the Lf part to avoid unneccsary flatten and compare.When we need to flatten the
trees, we convert the tree to a lazy list and call equal_list_lazy function. In equal_list_lazy function,we compare
the list lazily so that we can stop the compare process once we evaluated false and that also saves a lot of space.
On the contrary, the eager method convert the tree to a full list and then determine if the two trees
have same leaves.Therefore, it requires more space compared with the lazy method *)
