(*Group member:Yuanhao Ruan,Benjamin Osborn*)

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


let implode (cs: char list) : string =
String.concat "" (List.map  (String.make 1) cs)


(*let split f lst=
	let divide x (all_group,current_group)=
		if f x then (current_group::all_group,[]) else (all_group, x::current_group)  
		in let (groups,last)=List.fold_right divide lst ([],[])
		in (last::groups)*)

let d1 = "/home/ruanx054/csci2041/public-class-repo/Homework/Files/words-small.txt"
let d2 = "/home/ruanx054/csci2041/public-class-repo/Homework/Files/words-google-10000.txt"


(*let to_string alist =List.fold_right (fun x y->if List.length(x)>0 then (implode x)::y else y) alist []*)


(*let check string_lst=List.fold_right (fun x y->if String.length(x)=6&&List.mem(String.sub x 1 4) (string_lst) then x::y else y) string_lst []*)


(*let answers name= check (to_string(split (fun y->y='\n') (List.filter (fun x->x!=' ') (read_file (name)))))*)

(*let pretty_answers=List.map (fun x->(String.sub x 1 4,x))*)



(*split function to group the list elements by requirement*)
let split (f:'a->bool) (lst:'a list):'a list list=
	let divide x (all_group,current_group)=
		if f x then (current_group::all_group,[]) 
		       else (all_group, x::current_group)  
		       in let (groups,last)=
		       List.fold_right divide lst ([],[])
                       in (last::groups)


(*add type annotations and combined several functions together into a bigger answers function*)
let answers (name:string):string list=
	let file_list:char list=read_file(name)
	in let lines:char list=List.filter (fun x->x!=' ') file_list
	in let words:char list list=split (fun y->y='\n') lines
	in let words_list:string list=List.fold_right 
	(fun (x:char list) (y:string list)->if List.length(x)>0 then (implode x)::y else y) words []
	in List.fold_right 
	(fun (x:string) (y:string list)->if String.length(x)=6&&List.mem(String.sub x 1 4) (words_list) 
	then x::y 
	else y) words_list []

let pretty_answers (words_list:string list):(string *string) list=List.map (fun x->(String.sub x 1 4,x)) words_list
		
