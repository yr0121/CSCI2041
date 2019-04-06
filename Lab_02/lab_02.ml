let circle_circum_v1 radius=radius*.3.1415*.float_of_int(2)

let circle_circum_v2 radius= let pi=3.1415 in radius*.pi*.float_of_int(2)

let rec product xs=
	match xs with 
	| [] -> 1
	| x::rest->x*product rest

let rec sum_sqrdiffs xs=
	match xs with
	| [] -> 0
	| x::[] ->x*x
	| x1::(x2::[]) ->(x1-x2)*(x1-x2)
	| x3::x4::rest ->(x3-x4)*(x3-x4)+sum_sqrdiffs (x4::rest)

let distance (x1,y1) (x2,y2)=sqrt((x1-.x2)*.(x1-.x2)+.(y1-.y2)*.(y1-.y2))

let triangle_perimeter(x1,y1) (x2,y2) (x3,y3)=(distance(x1,y1) (x2,y2))+.(distance(x1,y1) (x3,y3))+.(distance(x2,y2) (x3,y3))


