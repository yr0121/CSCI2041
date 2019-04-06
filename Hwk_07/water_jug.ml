(* The amount of water is represented by int*int, the first int corresponds to the amount of water
in 4 gallon jag and the second int correspnds to the amount of water in 3 gallon jag *)
type state=int*int

let final s=s=(2,0)

let ok_state ((f,t):state):bool=
	f>=0&&f<=4&&t>=0&&t<=3

type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug

 let describe (four:int) (three:int) : string = 
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

let to_string (s:state):string=describe (fst s) (snd s)

let rec is_not_elem set v = not (List.mem v set)	


let update_state (s:state) (o:operation):state=
    match o with
	| Fill4GallonJugFromTap->(4,(snd s))
    | Fill3GallonJugFromTap->((fst s),3)
    | Empty4GallonJugOnGround->(0,(snd s))
    | Empty3GallonJugOnGround->(fst s,0)
    | Fill4GallonJugFrom3GallonJug->(4,((snd s)-4+(fst s)))
    | Fill3GallonJugFrom4GallonJug->(((fst s)-3+(snd s)),3)
    | Empty4GallonJugInto3GallonJug->(0,((snd s)+(fst s)))
    | Empty3GallonJugInto4GallonJug->(((fst s)+(snd s)),0)

let do_op (s:state)=
    match s with
    |(0,0)->[(Fill4GallonJugFromTap,describe 4 0);
            (Fill3GallonJugFromTap,describe 0 3)]
    |(4,0)->[(Fill3GallonJugFromTap,describe 4 3);
            (Fill3GallonJugFrom4GallonJug,describe 1 3)]
    |(0,3)->[(Fill4GallonJugFromTap,describe 4 3);
            (Empty3GallonJugInto4GallonJug,describe 3 0)]
    |(4,3)->[(Empty4GallonJugOnGround,describe 0 3);
            (Empty3GallonJugOnGround,describe 4 0)]
    |(1,3)->[(Fill4GallonJugFromTap,describe 4 3);
            (Empty4GallonJugOnGround,describe 0 3);
            (Empty3GallonJugOnGround,describe 1 0);
            (Fill4GallonJugFrom3GallonJug,describe 4 0);
            (Empty3GallonJugInto4GallonJug,describe 4 0)]
    |(3,0)->[(Fill4GallonJugFromTap,describe 4 0);
            (Fill3GallonJugFromTap,describe 3 3);
            (Fill3GallonJugFrom4GallonJug,describe 0 3);
            (Empty4GallonJugInto3GallonJug,describe 0 3)]
    |(1,0)->[(Fill4GallonJugFromTap,describe 4 0);
            (Fill3GallonJugFromTap,describe 1 3);
            (Empty4GallonJugInto3GallonJug,describe 0 1)]
    |(3,3)->[(Fill4GallonJugFromTap,describe 4 3);
            (Empty4GallonJugOnGround,describe 0 3);
            (Empty3GallonJugOnGround,describe 3 0);
            (Fill4GallonJugFrom3GallonJug,describe 4 2)]
    |(4,2)->[(Fill3GallonJugFromTap,describe 4 3);
            (Empty4GallonJugOnGround,describe 0 2);
            (Empty3GallonJugOnGround,describe 4 0);
            (Fill3GallonJugFrom4GallonJug,describe 3 3)]
    |(0,2)->[(Fill4GallonJugFromTap,describe 4 2);
            (Fill3GallonJugFromTap,describe 0 3);
            (Empty3GallonJugInto4GallonJug,describe 2 0)]
    |_->[]

exception Foundsolution of (operation*string) list

let play ()=
    let rec try_op st operation_list=
    if final st
    then raise (Foundsolution operation_list)
    else let valid_op=List.filter (is_not_elem operation_list) (do_op st)
    in
    let go_func () (m,s)=try_op (update_state st m) (operation_list@[(m,s)])
    in List.fold_left  go_func () valid_op
in try try_op (0,0) [];None
    with Foundsolution operation_list->Some operation_list