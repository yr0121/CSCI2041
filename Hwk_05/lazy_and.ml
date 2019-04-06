let rec ands (lst:bool list):bool=
	match lst with
	|[] -> true
	|x::xs->if not(x) then false else x&&ands xs
	