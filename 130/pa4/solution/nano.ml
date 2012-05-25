exception MLFailure of string

type binop =
  | Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | And
  | Or
  | Cons

type expr =
  | Const of int
  | True
  | False
  | NilExpr
  | Var of string
  | Bin of expr * binop * expr
  | If  of expr * expr * expr
  | Let of string * expr * expr
  | App of expr * expr
  | Fun of string * expr
  | Letrec of string * expr * expr

type value =
  | Int of int
  | Bool of bool
  | Closure of env * string option * string * expr
  | Nil
  | Pair of value * value

and env = (string * value) list

let binopToString op =
  match op with
    | Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v =
  match v with
  | Int i ->
      Printf.sprintf "%d" i
  | Bool b ->
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) ->
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) ->
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2)
  | Nil ->
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
    | Const i ->
        Printf.sprintf "%d" i
    | True ->
        "true"
    | False ->
        "false"
    | Var x ->
        x
    | Bin (e1,op,e2) ->
        Printf.sprintf "%s %s %s"
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) ->
        Printf.sprintf "if %s then %s else %s"
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) ->
        Printf.sprintf "let %s = %s in \n %s"
        x (exprToString e1) (exprToString e2)
    | App (e1,e2) ->
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) ->
        Printf.sprintf "fun %s -> %s" x (exprToString e)
    | Letrec (x,e1,e2) ->
        Printf.sprintf "let rec %s = %s in \n %s"
        x (exprToString e1) (exprToString e2)

(*********************** Some helpers you might need ***********************)

let rec fold f base args =
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) =
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) =
  let value = listAssoc (x, evn) in
    match value with
      | None    -> raise (MLFailure ("variable not bound: " ^ x))
      | Some x  -> x

let rec eval (evn,e) = match e with
  | Const v             ->  Int v
  | Var v               ->  lookup (v, evn)
  | False               ->  Bool false
  | True                ->  Bool true
  | Bin(e1, op, e2)     ->  let e1' = eval(evn, e1) in
                            let e2' = eval(evn, e2) in
                              (match (e1', e2') with
                              | (Int a, Int b)    -> (match op with
                                | Plus  -> Int (a + b)
                                | Minus -> Int (a - b)
                                | Mul   -> Int (a * b)
                                | Div   -> if b = 0 then raise (MLFailure ("Can not divide by zero"))
                                           else Int (a / b)
                                | Le    -> Bool (a <= b)
                                | Lt    -> Bool (a < b)
                                | Eq    -> Bool (a = b)
                                | Ne    -> Bool (a != b)
                                | _     -> raise (MLFailure ("Invalid operand")))
                              | (Bool a, Bool b)  -> (match op with
                                | Or    -> Bool (a || b)
                                | And   -> Bool (a && b)
                                | Eq    -> Bool (a = b)
                                | Ne    -> Bool (a != b)
                                | _     -> raise (MLFailure ("Invalid operand")))
                              | _                 -> raise (MLFailure ("Invalid operand")))
  | If (e1, e2, e3)     ->  let e1' = eval (evn, e1) in
                            (match e1' with
                              | Bool x  -> if x then eval (evn, e2) else eval (evn, e3)
                              | _       -> raise (MLFailure ("Bool expression expected")))
  | Let (s, e2, e3)     ->  let evn' = (s, eval (evn, e2)) :: evn in
                            eval (evn', e3)
  | Letrec (s, e2, e3)  ->  let evn' = (s, eval (evn, e2)) :: evn in
                            eval (evn', e3)

(**********************     Testing Code  ******************************)
