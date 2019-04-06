(* Adding function values, recursive let expressions, lambda
   expressions and function application. 
   This language extends the language in ``expr_let.ml``.
   Eric Van Wyk   
 *)
type expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id of string

  | App of expr * expr
  | Lambda of string * expr

  | LetRec of string * expr * expr
  | If of expr * expr * expr

and value
  = Int of int
  | Bool of bool
  | Closure of string * expr * environment
  | Ref of value ref

and environment = (string * value) list


let rec lookup (n: string) (env: environment) : value =
  match env with
  | [] -> raise (Failure ("Name \"" ^ n ^ "\" not in scope"))
  | (n',v)::_ when n' = n ->(match v with
  			     |Ref v-> !v
  			     |_->v
  			    )
  | _::rest -> lookup n rest
  

let rec freevars (e: expr) : string list = 
  match e with
  | Val _ -> []
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Let (n, dexpr, body) ->
     freevars dexpr @ (List.filter (fun n' -> n <> n') (freevars body))
  | Id n -> [n]
  | Sub (e1, e2) -> freevars e1 @ freevars e2
  | Mul (e1, e2) -> freevars e1 @ freevars e2
  | Div (e1, e2) -> freevars e1 @ freevars e2
  | Lt(e1, e2) -> freevars e1 @ freevars e2
  | Eq (e1, e2) -> freevars e1 @ freevars e2
  | And (e1, e2) -> freevars e1 @ freevars e2
  | Not e1 ->freevars e1
  | App(e1,e2)->freevars e1 @ freevars e2
  | Lambda(str,e1)->List.filter (fun n' -> str <> n') (freevars e1)
  | LetRec(str,e1,e2)->List.filter (fun n' -> str <> n') (freevars e1@freevars e2)
  | If(e1,e2,e3)->freevars e1 @ freevars e2 @freevars e3


let rec eval (env: environment) (e: expr) : value = 
  match e with
  | Val v -> v
  | Let (n, dexpr, body) ->
      let v = eval env dexpr in
      eval ( (n,v)::env ) body
  | Id n -> lookup n env
  | Add (e1, e2) -> 
    ( match eval env e1 , eval env e2 with
      | Int i1, Int i2 -> Int (i1 + i2)
      | _, _ -> raise (Failure "incompatible values on Add")
    )
  | Sub (e1, e2) -> 
     ( match eval env e1 , eval env e2 with
       | Int i1, Int i2 -> Int (i1 - i2)
       | _, _ -> raise (Failure "incompatible values on Sub")
     )
  | Mul (e1, e2) -> 
     ( match eval env e1 , eval env e2 with
       | Int i1, Int i2 -> Int (i1 * i2)
       | _, _ -> raise (Failure "incompatible values on Mul")
     )
  | Div (e1, e2) -> 
     ( match eval env e1 , eval env e2 with
       | Int i1, Int i2 -> Int (i1 / i2)
       | _, _ -> raise (Failure "incompatible values on Div")
     )
  | Lt (e1, e2) -> 
     (match eval env e1 , eval env e2 with
       | Int i1, Int i2 -> Bool (i1 < i2)
       | _, _ -> raise (Failure "incompatible values on Lt")
     )
  | And (e1, e2) -> 
     ( match eval env e1 , eval env e2 with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _, _ -> raise (Failure "incompatible values on And")
     )
  | Eq (e1, e2) -> 
     ( match eval env e1 , eval env e2 with
       | Int i1, Int i2 -> Bool (i1 = i2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _, _ -> raise (Failure "incompatible values on Eq")
     )
  | Not e1 ->
     ( match eval env e1 with
       | Bool b -> Bool (not b)
       | _ -> raise (Failure "incompatible value on Not")
     )
  | App (e1,e2)->
  	 ( match eval env e1,eval env e2 with
  	   |Closure(str,e',env'),v->eval ((str,v)::env'@env) e'
  	   |_->raise (Failure "incompatible value on APP")
  	 )	
  | Lambda (str,e)->Closure(str,e,env)
  | LetRec (str,e1,e2)->
  	( match e1 with
  	   |Lambda (str',e)->let recRef=ref (Int 999) in
  	   		     let step1=eval ((str,Ref recRef)::env) e1
  	   		     in let()=recRef:=step1 in
  	   		     eval ((str,step1)::env) e2
  	   |_->raise (Failure "incompatible value on LetRec")
  	 )	
  | If (e1,e2,e3)->
  	( match eval env e1 with
  	   |Bool true->eval env e2
  	   |Bool false->eval env e3
  	   |_->raise (Failure "incompatible value on If")
  	 )	


let evaluate e = eval [] e

let rec serialize (e:expr):string=
  match e with
  | Val v->(match v with
	    |Int i->"Val (Int "^string_of_int i^")"
	    |Bool b->"Val Bool "^string_of_bool b^")"
	    |_->raise (Failure "Will only serialize integer and Boolean values")
	   )
  | Add(e1,e2)->"Add ("^serialize e1^", "^serialize e2^")"
  | Sub(e1,e2)->"Sub ("^serialize e1^", "^serialize e2^")"
  | Mul(e1,e2)->"Mul ("^serialize e1^", "^serialize e2^")"
  | Div(e1,e2)->"Div ("^serialize e1^", "^serialize e2^")"
  | Lt(e1,e2)->"Lt ("^serialize e1^", "^serialize e2^")"
  | Eq(e1,e2)->"Eq ("^serialize e1^", "^serialize e2^")"
  | And(e1,e2)->"And ("^serialize e1^", "^serialize e2^")"
  | Not e1->"Not ("^serialize e1^")"
  | Let (str,e1,e2)->"Let (\""^str^"\", "^serialize e1^", "^serialize e2^")"
  | Id x->"Id \""^x^"\""
  | App(e1,e2)->"App ("^serialize e1^", "^serialize e2^")"
  | Lambda(str,e1)->"Lambda (\""^str^"\", "^serialize e1^")"
  | LetRec(str,e1,e2)->"LetRec (\""^str^"\", "^serialize e1^", "^serialize e2^")"
  | If(e1,e2,e3)->"If ("^serialize e1^", "^serialize e2^", "^serialize e3^")"

let rec unparse (e:expr):string=
   match e with
   | Val v->(match v with
	    |Int i->string_of_int i
	    |Bool b->string_of_bool b
	    |_->raise (Failure "Will only serialize integer and Boolean values")
	    )
   | Add(e1,e2)->"("^unparse e1^" + "^unparse e2^")"
   | Sub(e1,e2)->"("^unparse e1^" - "^unparse e2^")"
   | Mul(e1,e2)->"("^unparse e1^" * "^unparse e2^")"
   | Div(e1,e2)->"("^unparse e1^" / "^unparse e2^")"
   | Lt(e1,e2)->"("^unparse e1^" < "^unparse e2^")"
   | Eq(e1,e2)->"("^unparse e1^" = "^unparse e2^")"
   | And(e1,e2)->"("^unparse e1^" && "^unparse e2^")"
   | Not e1->"(not"^unparse e1^")"
   | Let (str,e1,e2)->"(let "^str^" = "^unparse e1^" in "^unparse e2^")"
   | Id x->x
   | App(e1,e2)->"("^unparse e1^" "^unparse e2^")"
   | Lambda(str,e1)->"(fun "^str ^"-> "^unparse e1^")"
   | LetRec(str,e1,e2)->"("^str^unparse e1^unparse e2^")"
   | If(e1,e2,e3)->"(If "^unparse e1^" then "^unparse e2^"else "^unparse e3
    

(* Some sample expressions and their values*) 
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = evaluate e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = evaluate e2

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* ``let y = 5 in let x = y + 5 in x + y'' *)
let e6 = Let ("y", 
              Val (Int 5), 
              Let ("x", 
                   Add (Id "y", Val (Int 5)), 
                   Add (Id "x", Id "y")
                  )
             )

let () =
  assert (serialize e1 = "Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))");
  assert (serialize e6 = 
            "Let (\"y\", Val (Int 5), Let (\"x\", " ^ 
              "Add (Id \"y\", Val (Int 5)), Add (Id \"x\", Id \"y\")))")


(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

let () =
  assert (evaluate e1 = Int 7);
  assert (evaluate e2 = Int 7);
  assert (evaluate e3 = Bool true);
  assert (evaluate e4 = Bool false);
  assert (evaluate e5 = Bool true);
  assert (evaluate e6 = Int 15);
  assert (evaluate e7 = Bool true)


(* increment *)
let inc = Lambda ("n", Add(Id "n", Val (Int 1)))

let add = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                 )
let inc' = App (add, Val (Int 1))

(* The add2 closure *)
let add2app =
  Let ("add2",
       Let ("two", Val (Int 2), Lambda ("x", Add (Id "x", Id "two"))),
       App (Id "add2", Val (Int 4)))

let () =
  assert (evaluate (App (inc, Val (Int 4))) = Int 5);
  assert (evaluate (Add (Val (Int 2), Val (Int 3))) = Int 5);
  assert (evaluate (App (inc', Val (Int 4))) = Int 5);
  assert (evaluate add2app = Int 6)


(* sumToN *)
let sumToN : expr =
    LetRec ("sumToN", 
            Lambda ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n", 
                             App (Id "sumToN", 
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )

(* factorial *)
let fact : expr =
    LetRec ("fact", 
            Lambda ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 1),
                        Mul (Id "n", 
                             App (Id "fact", 
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "fact"
           )

(* Assert expressions to test our functions. *)
let () =
  assert (evaluate (App (sumToN, Val (Int 4))) = Int 10);
  assert (evaluate (App (sumToN, Val (Int 10))) = Int 55);
  assert (evaluate (App (sumToN, Val (Int 100))) = Int 5050);
  assert (evaluate (App (fact, Val (Int 0))) = Int 1);
  assert (evaluate (App (fact, Val (Int 1))) = Int 1);
  assert (evaluate (App (fact, Val (Int 2))) = Int 2);
  assert (evaluate (App (fact, Val (Int 4))) = Int 24)



(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)
let () =
  print_endline ("Success! All tests passed.")
