(*test if a number is even number or not*)
let rec even x=
	match x with
	|0->true
	|1->false
	|_-> even (x mod 2)

(*greatest common divisor*)	
let rec euclid a b=
	if a=b then a
	else if a<b then euclid a (b-a)
	else euclid (a-b) b

(*use the euclid function above to simplify a fractional number*)
let frac_simplify (x1,x2)=x1/(euclid x1 x2),x2/(euclid x1 x2)

(*return the maximum number of a int list*)
let rec max (x:int list) =
	match x with
	|[]->raise (Failure "Input list must not be empty")
	|xs::[]->xs
	|hd::rest->if(max rest)>hd then (max rest) else hd

(*take the required number of elements from the list, if then passed elements is invalid, return empty list*)
let rec take x lst=
	match lst with 
	|[]->[]
	|xs::rest->if x<=0 then [] else xs::take (x-1) rest
