(*Name:Yuanhao Ruan,Partner name:Hanzhang Wu *)

(*No change*)
let circle_circum_v1 radius=radius*.3.1415*.float_of_int(2)

(*No change*)
let circle_circum_v2 radius= let pi=3.1415 in radius*.pi*.float_of_int(2)

(*No change*)
let rec product xs=
	match xs with 
	| [] -> 1
	| x::rest->x*product rest
	
(*with a list with fewer than 2 elements,raise an exception*)
let rec sum_sqrdiffs xs=
	match xs with
	| x1::(x2::[]) ->(x1-x2)*(x1-x2)
	| x3::x4::rest ->(x3-x4)*(x3-x4)+sum_sqrdiffs (x4::rest)
	|_->raise (Failure "sum_sqrdiffs input list needs at least two elements")

(*use an exponential operator*)
let distance (x1,y1) (x2,y2)=sqrt((x1-.x2)**2.0+.(y1-.y2)**2.0)

(*change pevious expressions to a more concise expression*)
let triangle_perimeter v1 v2 v3=(distance v1 v2)+.(distance v2 v3)+.(distance v3 v1)


