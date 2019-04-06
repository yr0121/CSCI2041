(*read file*)
let read_file (file_name: string) : char list =
  let ic = open_in file_name 
  in 
  let rec read_chars ic =
    try 
      let next_char = input_char ic
      in next_char :: read_chars ic
    with 
      _ -> [] 
  in read_chars ic

(*convert a char list to string*)
let implode (cs: char list) : string =
String.concat "" (List.map  (String.make 1) cs)

(*split function to group the list elements by requirement*)
let split f lst=
	let divide x (all_group,current_group)=
		if f x then (current_group::all_group,[]) else (all_group, x::current_group)  
		in let (groups,last)=List.fold_right divide lst ([],[])
		in (last::groups)

let d1 = "/home/ruanx054/csci2041/public-class-repo/Homework/Files/words-small.txt"
let d2 = "/home/ruanx054/csci2041/public-class-repo/Homework/Files/words-google-10000.txt"

(*a helper function that convert char list list to string list*)
let to_string alist =List.fold_right (fun x y->if List.length(x)>0 then (implode x)::y else y) alist []

(*a helper function that check if the element in list has a length of 6 and the substring contains elements in list*)
let check string_lst=List.fold_right (fun x y->if String.length(x)=6&&List.mem(String.sub x 1 4) (string_lst) then x::y else y) string_lst []

(*read the file and output the strings with 6 letters whose substring also in the list*)
let answers name= check (to_string(split (fun y->y='\n') (List.filter (fun x->x!=' ') (read_file (name)))))

(*convert 6 letter string list to a tuple list with the first argument the 4 letter words and the second argument the string*)
let pretty_answers=List.map (fun x->(String.sub x 1 4,x))

		
