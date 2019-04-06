open Intervals

module Rationals_comparable : (Comparable with type t = int*int) = struct
  type t = int*int
  let rec gcd a b =
   if b=0 then a
   else gcd b (a mod b)
  
  let compare (x1,y1) (x2,y2)=compare (float_of_int(x1)/.float_of_int(y1)) (float_of_int(x2)/.float_of_int(y2))
  
  let to_string (input:t):string=
  	match input with
  	|(x1,x2)->
  	string_of_int (x1/(gcd x1 x2))^"/"^string_of_int (x2/(gcd x1 x2))
    


end

module Rational_interval = Make_interval(Rationals_comparable)



