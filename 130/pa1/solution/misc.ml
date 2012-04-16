(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
   Returns the sum of the integer elements of a given list.
   (see the digits function below for an example of what is expected)
*) 

let rec sumList l = 
  match l with
  | []    -> 0
  | h::t  -> h + sumList t


(* digitsOfInt : int -> int list 
   Returns the list of digits of given integer order in which they appear.
   Should return empty list if given number is not positive.
 *)

let rec digitsOfInt n = 
  if n <= 0 then []
  else
    let m = n mod 10 in
      let d = n / 10 in
        digitsOfInt d @ [m]


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
  Returns the number of additions required to obtain a single digit from a given number.
  Should return 0 if given number is not positive.
*)

let additivePersistence n = 
  if n <= 0 then 0
  else
    let rec additivePersistence' (n, r) =
      let l = digits n in
        if n <= 9 then r
        else additivePersistence' (sumList l, r + 1)
    in additivePersistence' (n, 0)

(* digitalRoot : int -> int
  Returns the result of successive additions of digits of given number.
  Should return 0 if given number is not positive.
*)

let digitalRoot n = 
  if n <= 0 then 0
  else
    let rec digitalRoot' n =
      let l = digits n in
        if n <= 9 then n
        else digitalRoot' (sumList l)
    in digitalRoot' n

(* listReverse : a' list -> a' list
  Returns the list of elements of given list in the reversed order in which they appear.
*)

let listReverse l = 
  let rec listReverse' (acc, l) =
    match l with
    | []    -> acc
    | h::t  -> listReverse' (h::acc, t)
  in listReverse' ([], l)

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* listReverse : string -> bool
  Returns whether given string reads the same from left-to-right and right-to-left.
*)

let palindrome w = 
  let lw = explode w in
    let rlw = listReverse lw in
      lw = rlw
