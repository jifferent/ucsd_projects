(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

let sqsum xs =
  let f a x = a + x * x in
  let base = 0 in
    List.fold_left f base xs

let pipe fs =
  let f a x = fun y -> x (a y) in
  let base  = fun x -> x in
    List.fold_left f base fs

let rec sepConcat sep sl = match sl with
  | [] -> ""
  | h :: t ->
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l

let stringOfList f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

let rec clone x n =
  if n <= 0
  then []
  else x :: clone x (n - 1)

let rec padZero l1 l2 =
  let n1 = List.length l1 in
  let n2 = List.length l2 in
    if n1 = n2
    then (l1, l2)
    else
      if n1 < n2
      then ((clone 0 (n2 - n1)) @ l1, l2)
      else (l1, (clone 0 (n1 - n2)) @ l2)

let rec removeZero l = match l with
  | [] -> []
  | h::t -> if h == 0
            then removeZero t
            else h::t

let bigAdd l1 l2 =
  let add (l1, l2) =
    let f a x =
      let (carry, result) = a in
      let (x1, x2)        = x in
      let res             = x1 + x2 + carry in
      let newCarry        = res / 10 in
        match result with
          | []      -> (newCarry, newCarry :: (res mod 10) :: [])
          | h :: t  -> (newCarry, newCarry :: (res mod 10) :: t)
    in
    let base = (0, []) in
    let args = List.combine (List.rev l1) (List.rev l2) in
    let (_, res) = List.fold_left f base args in
      res
  in
    removeZero (add (padZero l1 l2))

let rec mulByDigit i l = match i with
  | 0 -> [0]
  | _ -> bigAdd l (mulByDigit (i - 1) l)

let bigMul l1 l2 =
  let f a x =
    let (i, acc)  = a in
    let (_, n)    = x in
    let res       = bigAdd (mulByDigit n l1 @ clone 0 i) acc in
      (i + 1, res)
  in
  let base = (0, []) in
  let args = List.combine (clone l1 (List.length l2)) (List.rev l2) in
  let (_, res) = List.fold_left f base args in
    res
