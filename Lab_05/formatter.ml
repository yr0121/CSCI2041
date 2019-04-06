(*Group member:Yuanhao Ruan,Benjamin Osborn*)
let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "
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

let split f lst=
	let divide x (all_group,current_group)=
		if f x then (current_group::all_group,[]) else (all_group, x::current_group)  
		in let (groups,last)=List.fold_right divide lst ([],[])
		in (last::groups)

let d1 = "/home/ruanx054/csci2041/public-class-repo/Homework/Files/words-small.txt"
let d2 = "/home/ruanx054/csci2041/public-class-repo/Homework/Files/words-google-10000.txt"

(*convert a string to char list*)
let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i = 
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

(*let to_string alist= List.fold_right (fun x y->if List.length(x)>0 then (implode x)::y else y) alist []*)

(*a helper function that takes a list and the reuqired length as arguments and groups the string elements in the list by length
let group lst length=
  let accum = ( "", "", 0 )
  in
  let f (all_groups, current_group, size) x =
    if(size=0) then (all_groups,x,String.length(x))
    else if(String.length (current_group)+String.length(x)>length-1) then (all_groups^current_group^"\n",x,String.length(x) )
    else (all_groups,(current_group^" "^x),size+1+String.length(x))
    in let (groups,last,_)=
   List.fold_left f accum lst in (groups^last)*)

(*format function takes the input string and length as argument and remove all the '\n' and '\t'
 and split by white space, then the result was converted to string list and use the group function to
 achieve the requirement
let format input length=group (to_string (split (fun y->y=' ') (List.filter (fun x->x!='\n') (List.filter (fun z->z!='\t') (explode (String.trim (input))))))) length*)



let format (input:string) (length:int):string=
	let words_charlist:char list=explode (String.trim(input)) in
	let clear_charlist:char list=List.filter (fun x->x!='\t') words_charlist in
	let final_charlist:char list=List.filter (fun y->y!='\n') clear_charlist in
	let split_list:char list list=split (fun z->z=' ') final_charlist in
	let words:string list= List.fold_right 
		(fun (a:char list) (b:string list)->if List.length(a)>0 
		then (implode a)::b else b)  split_list [] in
	let accum:(string*string*int)= ( "", "", 0 )
  	in
  		let f ((all_groups:string), (current_group:string), (size:int)) (x:string):string*string*int =
    			if(size=0) then (all_groups,x,String.length(x))
    			else if(String.length (current_group)+String.length(x)>length-1) 
			then (all_groups^current_group^"\n",x,String.length(x) )
    			else (all_groups,(current_group^" "^x),size+1+String.length(x))
    			in let (groups,last,_)=
  	 List.fold_left f accum words in (groups^last)

	

let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."
 

