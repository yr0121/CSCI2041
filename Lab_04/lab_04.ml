let length lst=List.fold_left (fun accum _ -> 1 + accum) 0 lst
let andf=List.fold_left (fun accum ele->accum&&ele) true
let orf=List.fold_left (fun accum ele->accum||ele) false
let is_elem ele lst=List.fold_left (fun accum x->if x=ele then true else accum) false lst
let rev lst=List.fold_left(fun accum x->x::accum) [] lst
let ascii_sum lst=List.fold_left(fun accum x->accum+Char.code(x)) 0 lst
let lebowski lst =List.fold_right(fun x y->if x='.' then [','; ' '; 'd'; 'u'; 'd'; 'e'; '.']@y else x::y) lst []