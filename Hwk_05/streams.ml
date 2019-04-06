(* The code below is from Professor Eric Van Wyk. *)

(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee

(* functions taken from sample programs lazy.ml *)
let rec from n = 
  (* print_endline ("step " ^ string_of_int n) ;  *)
  Cons ( n, 
         delay (fun () -> from (n+1) )
       )

let nats = from 1

let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n:int) (s : 'a stream) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

 let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) 
                                           (demand tl2) ) )
 let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

  let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = delay (fun () -> filter p (demand tl) )
     in if p hd
        then Cons (hd, rest)
        else demand rest

(* The code below is from Yuanhao Ruan *)
let rec cubes_from (input:int):int stream=
	Cons (input*input*input,delay (fun()->cubes_from (input+1)))

let cubes_from_zip (input:int):int stream=
	zip ( * ) (from input) (zip ( * ) (from input) (from input))

let cubes_from_map (input:int):int stream=
	map (fun x->x*x*x) (from input)

let rec drop (num:int) (input_stream:'a stream):'a stream=
	match num,input_stream with
	|0,_->input_stream
	|_,Cons(v,tl)->drop (num-1) (demand tl)

let rec drop_until (f:'a->bool) (input_stream:'a stream):'a stream=
	match input_stream with
	|Cons(v,tl)->if f v 
               then Cons(v,delay (fun()->drop_until f (demand tl))) 
               else drop_until f (demand tl)

let rec foldr (f:'a->'b lazee->'b) (input:'a stream):'b=
    match input with
    |Cons(v,tl)->f v (delay (fun ()->foldr f (demand tl)))

(* foldr take a function f whihc has a typpe of 'a->'b lazee->'b and a input 'a streeam and returns
a type of 'b.This function has the similar meaning with the foldr list function 
(('a -> 'b -> 'b) -> 'a list -> 'b -> 'b) except that it is 
performed on 'a stream. I think stream can be viewed as infinite list.
First,this foldr takes some function that takes two argument 'a and 'b lazee.
Because the input stream is'a stream so the function should take one argument with type of 'a.Similar 
to the foldr list function,it should take another argument as well.Because we want to lazily obtain the result
then we can set the second argument of 'b lazee.When the first argument satisfy the condition we will evaluate
the lazee part or we will terminate the process and return the output which has a type 'b.Since it is a foldr 
functuon, the return ype should be a value which has the same type of the return type of f, so it should be 'b  *)

let and_fold (input:bool stream):bool=
  foldr ( fun x y->if x then (demand y) else false ) input

let sum_positive_prefix (input:int stream):int=
  foldr (fun a b->if a>0 then (a+(demand b)) else 0) input

let ns : int stream = zip ( - ) (from 1000) (cubes_from 1)

let are_positive ns = map (fun n -> n > 0) ns

let ns_positive : bool stream = are_positive ns

let rec sieve (input:int stream):int stream=
    match input with
    |Cons(v,tl)->let sift (n:int) (s:int stream):int stream=
                filter (fun x->x mod n <>0) s
				        in Cons (v,delay (fun()->sieve (sift v (demand tl))))