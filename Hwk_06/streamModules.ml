open LazeeModules


module type StreamSig = sig
  type 'a lazee
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a 

  type 'a t = Cons of 'a * 'a t lazee

  val head:'a t->'a
  val tail:'a t->'a t
  val take:int->'a t->'a list
  val filter:('a->bool)->'a t->'a t
  val map:('a->'b)->'a t->'b t
  val zip:('a->'b->'c)->'a t->'b t->'c t
  val from:int->int t
  val cubes_from:int->int t
  val drop:int->'a t->'a t
  val drop_until:('a->bool)->'a t->'a t
  val foldr:('a->'b lazee->'b)->'a t->'b
  val sieve:int t->int t

end

module Stream (L: LazeeSig) : StreamSig = struct
  type 'a lazee = 'a L.t
  let delay = L.delay
  let demand = L.demand
  type 'a t = Cons of 'a * 'a t L.t
  let head t=match t with
  	|Cons(v,_)->v

  let tail t=match t with
  	|Cons(_,tl)->L.demand tl

 let rec take n t =
 	match n, t with
 	| 0, _ -> []
 	| _, Cons (v, tl) -> v :: take (n-1) (L.demand tl)

 let rec filter p t=
 	match t with
 	|Cons(hd,tl)->
 		let rest=L.delay (fun()->filter p (L.demand tl))
 		in if p hd
 			then Cons(hd,rest)
 		else L.demand rest
 let rec map f t=
 	match t with
 	|Cons (hd,tl)->
 		Cons(f hd,L.delay (fun()->map f (L.demand tl)))

 let rec zip f t1 t2=
 	match t1,t2 with
 	|Cons(hd1,tl1),Cons(hd2,tl2)->
 		Cons(f hd1 hd2,L.delay (fun()->zip f (L.demand tl1) (L.demand tl2)))

 let rec from n=Cons(n,L.delay (fun()->from(n+1)))

 let rec cubes_from n=
	Cons (n*n*n,L.delay (fun()->cubes_from (n+1)))

 let rec drop n t=
	match n,t with
	|0,_->t
	|_,Cons(v,tl)->drop (n-1) (L.demand tl)

let rec drop_until f t=
	match t with
	|Cons(v,tl)->if f v 
               then Cons(v,L.delay (fun()->drop_until f (L.demand tl))) 
               else drop_until f (L.demand tl)

 let rec foldr f t=
    match t with
    |Cons(v,tl)->f v (L.delay (fun ()->foldr f (L.demand tl)))

 let rec sieve t=
    match t with
    |Cons(v,tl)->let sift n s=
                filter (fun x->x mod n <>0) s
				        in Cons (v,delay (fun()->sieve (sift v (demand tl))))

end