(* CSE 130 PA 4. Autotester *)

#load "unix.cma";;

let wrap f g = if false then f g else failwith "imperative features used"

let key = "" (* change *)
let prefix130 = "130" (* change *)
let print130 s = print_string (prefix130^">>"^s^"\n")

let edits = ref 0
let edit_penalty = 10
let deduct s p = (print130 (s^" (-"^(string_of_int p)^")"); edits := p + !edits)
let edit s = deduct ("Compile error: "^s) edit_penalty

let __ce = (:=)
let __ref = ref
let __deref = (!)
let (:=) = wrap __ce
let ref = wrap __ref
let (!) = wrap __deref;;

let (:=) = __ce
let ref = __ref
let (!) = __deref;;


exception ErrorCode of string

type result = Pass | Fail | ErrorCode of string

let score = ref 0
let max = ref 0
let timeout = 5

exception TimeOutException

let runWTimeout (f,arg,out,time) = 
  try 
    if compare (f arg) out = 0 then Pass else Fail
  with e -> 
    (print130 ("Uncaught Exception: "^(Printexc.to_string e));
     ErrorCode "exception")
 

exception TestException
let testTest () =
  let testGood x = 1 in
  let testBad x = 0 in 
  let testException x = raise TestException in
  let rec testTimeout x = testTimeout x in
    runWTimeout(testGood,0,1,5) = Pass &&  
    runWTimeout(testBad,0,1,5) = Fail &&  
    runWTimeout(testException,0,1,5) = ErrorCode "exception" && 
    runWTimeout(testTimeout,0,1,5) = ErrorCode "timeout"


let runTest (f,arg,out,points,name) =
  let _ = max := !max + points in
  let outs = 
	match runWTimeout(f,arg,out,timeout) with 
	    Pass -> (score := !score + points; "[pass]")
 	  | Fail -> "[fail]"
	  | ErrorCode e -> "[error: "^e^"]"  in
  name^" "^outs^" ("^(string_of_int points)^")"

(* explode : string -> char list *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

let implode cs = 
  String.concat "" (List.map (String.make 1) cs)

let drop_paren s = 
  implode (List.filter (fun c -> not (List.mem c ['(';' ';')'])) (explode s))

let eq_real p (r1,r2) = 
  (r1 -. r2) < p || (r2 -. r1) < p

let parseString s =
  NanoParse.exp NanoLex.token (Lexing.from_string s)

let expectMLFailure f x =
  try
    f x;false
  with Nano.MLFailure _ ->
    true

let emptyClosureEnv f x =
  match f x with
    Nano.Closure(_,a,b,c) -> Nano.Closure([],a,b,c)
  | x -> x

let runAllTests () =
  let _ = (score := 0; max := 0) in
  let report =
  [
    runTest (parseString, "true",Nano.True,1,"1a - \"true\"");
    runTest (parseString, " 123\n\t",Nano.Const 123,1,"1a - \" 894\\n\\t\"");
    runTest (parseString, "x",Nano.Var "x",1,"1a - \"Z\"");

    runTest (parseString, "let x = 5 in x",Nano.Let ("x",Nano.Const 5,Nano.Var "x"),1,"1b - let x = 5 in x");
    runTest (parseString, "fun x -> 5",Nano.Fun ("x",Nano.Const 5),1,"1b - fun x -> 5");
    runTest (parseString, "if a then b else c",Nano.If (Nano.Var "a",Nano.Var "b",Nano.Var "c"),1,"1b - if a then b else c");
    
    runTest (parseString, "x+2",  Nano.Bin(Nano.Var "x",Nano.Plus,Nano.Const 2),1,"1c - x+2");
    runTest (parseString, "x<=2", Nano.Bin(Nano.Var "x",Nano.Le,Nano.Const 2),1,"1c - x<=2");
    runTest (parseString, "x&&2", Nano.Bin(Nano.Var "x",Nano.And,Nano.Const 2),1,"1c - x&&2");
    
    runTest (parseString,"1+(2*(3))",Nano.Bin(Nano.Const 1,Nano.Plus,Nano.Bin(Nano.Const 2,Nano.Mul,Nano.Const 3)),1,"1d - 1+(2*(3))");
    runTest (parseString,"f x",Nano.App(Nano.Var "f",Nano.Var "x"),1,"1d - f x");
	runTest (parseString,"e::f", Nano.Bin(Nano.Var "e", Nano.Cons, Nano.Var "f"), 1, "1e - e::f");
    ]
@ 
  let env1 = [("c0",Nano.Int 0);
              ("c1",Nano.Int 1);
              ("c2",Nano.Int 2);
              ("c3",Nano.Int 3);
              ("c0",Nano.Int 4);
              ("c1",Nano.Int 5)] in
  let env2 = env1 @ [("bt",Nano.Bool true);("bf",Nano.Bool false)] in
  [
  runTest (Nano.eval,(env1,Nano.Var "c1"),Nano.Int 1,2,"2a - c1 (evn is [c0=0,c1=1,c2=2,c3=3,c0=4,c1=5,bt=true,bf=false])");
  runTest (Nano.eval,(env1,Nano.Bin(Nano.Bin(Nano.Const 20,Nano.Minus,Nano.Var "c1"),
           Nano.Mul,Nano.Bin(Nano.Var "c2",Nano.Plus,Nano.Var "c3"))),
    Nano.Int 95,3,"2a - (20-c1)*(c2+c3)");

  runTest (expectMLFailure Nano.eval,(env2,Nano.Bin(Nano.Var "bt",Nano.Eq,Nano.Var "c3")),true,1,"2b - bt=c3");
  runTest (expectMLFailure Nano.eval,(env2,Nano.Bin(Nano.Var "bt",Nano.Or,Nano.Var "c3")),true,1,"2b - bt||c3");
  runTest (Nano.eval,([],Nano.Let("x",Nano.Const 4,Nano.Var "x")),Nano.Int 4,1,"2c - let x = 4 in x");
  runTest (Nano.eval,([],Nano.App(Nano.Fun("x",Nano.Bin(Nano.Var "x",Nano.Mul,Nano.Var "x")),Nano.Const 5)),
		  Nano.Int 25,2,"2d - (fun x->x*x) 5");

  (* let rec even = fun x -> let rec odd = fun x -> if x = 0 then false else even (x-1) in if x = 0 then true else odd (x-1) in even 23 *)
  runTest (Nano.eval,([],Nano.Letrec("even",Nano.Fun("x",Nano.Letrec("odd",
											Nano.Fun("x",Nano.If(Nano.Bin(Nano.Var "x",Nano.Eq,Nano.Const 0),
														  Nano.False,Nano.App(Nano.Var "even",
																		  Nano.Bin(Nano.Var "x",Nano.Minus,Nano.Const 1)))),
											Nano.If(Nano.Bin(Nano.Var "x",Nano.Eq,Nano.Const 0),
												  Nano.True,Nano.App(Nano.Var "odd",
																 Nano.Bin(Nano.Var "x",Nano.Minus,Nano.Const 1))))),
		Nano.App(Nano.Var "even",Nano.Const 23))),Nano.Bool false,3,"2e - let rec even = fun x -> let rec odd = fun x -> if x = 0 then false else even (x-1) in if x = 0 then true else odd (x-1) in even 23");
   ]

    in
    let _ = score := !score - !edits in
    let _ = score := if !score < 0 then 0 else !score in
    let s = Printf.sprintf "Results: Score/Max = %d / %d \n" !score !max in
    let _ = List.iter print130 (report@([s])) in
    (!score,!max)


let _ = runAllTests ()

let _ = print130 ("Compiled"^key^"\n")
    
