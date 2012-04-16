(* CSE 130 PA 1. Autotester *)

#use "misc.ml"
#use "tester.ml" 

let sampleTests =
  [
  (fun () -> mkTest
    sumList
    [1;2;3;4]
    10
    "sample: sumList 1"
  );
  (fun () -> mkTest 
    sumList 
    [1;-2;3;5] 
    7 
    "sample: sumList 2"
  ); 
  (fun () -> mkTest 
    sumList 
    [1;3;5;7;9;11]
    36 
    "sample: sumList 3"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    3124 
    [3;1;2;4] 
    "sample: digitsOfInt 1"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    352663 
    [3;5;2;6;6;3] 
    "sample: digitsOfInt 2"
  ); 
  (fun () -> mkTest 
    digits
    31243
    [3;1;2;4;3] 
    "sample: digits 1"
  ); 
  (fun () -> mkTest 
    digits
    (-23422)
    [2;3;4;2;2]
    "sample: digits 2"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    9876 
    2 
    "sample: additivePersistence1"
  ); 
  (fun () -> mkTest 
    digitalRoot 
    9876 
    3 
    "sample: digitalRoot"
  ); 
  (fun () -> mkTest 
    listReverse
    [1;2;3;4] 
    [4;3;2;1]
    "sample: reverse 1"
  ); 
  (fun () -> mkTest 
    listReverse 
    ["a";"b";"c";"d"]
    ["d";"c";"b";"a"] 
    "sample: rev 2"
  ); 
  (fun () -> mkTest 
    palindrome 
    "malayalam" 
    true
    "sample: palindrome 1"
  ); 
  (fun () -> mkTest 
    palindrome 
    "myxomatosis" 
    false
    "sample: palindrome 2"
  )] 

(*130*************************************************************)
(*130**************** BEGIN MODIFY *******************************)
(*130*************************************************************)

let yourTests = 
  [ 
  (fun () -> mkTest 
    sumList 
    ([10;20;30;40]) 
    (100) 
    "sumList 1"
  ); 
  (fun () -> mkTest 
    sumList 
    ([-10;-20;-30;-40]) 
    (-100) 
    "sumList 2"
  ); 
  (fun () -> mkTest 
    sumList 
    ([0;5;-5]) 
    (0) 
    "sumList 3"
  ); 
  (fun () -> mkTest 
    sumList 
    ([100;-100;100]) 
    (100) 
    "sumList 4"
  ); 
  (fun () -> mkTest 
    sumList 
    ([]) 
    (0) 
    "sumList 5"
  );

  (fun () -> mkTest 
    digitsOfInt 
    (0) 
    ([]) 
    "digitsOfInt 1"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    (01) 
    ([1]) 
    "digitsOfInt 2"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    (100) 
    ([1;0;0]) 
    "digitsOfInt 3"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    (123) 
    ([1;2;3]) 
    "digitsOfInt 4"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    (-1) 
    ([]) 
    "digitsOfInt 5"
  ); 

  (fun () -> mkTest 
    digits
    (100) 
    ([1;0;0]) 
    "digits 1"
  ); 
  (fun () -> mkTest 
    digits
    (0) 
    ([]) 
    "digits 2"
  ); 
  (fun () -> mkTest 
    digits
    (-1) 
    ([1]) 
    "digits 3"
  ); 
  (fun () -> mkTest 
    digits
    (1234567890) 
    ([1;2;3;4;5;6;7;8;9;0]) 
    "digits 4"
  ); 
  (fun () -> mkTest 
    digits
    (0987654321) 
    ([9;8;7;6;5;4;3;2;1]) 
    "digits 5"
  ); 
 
  (fun () -> mkTest 
    additivePersistence 
    (123) 
    (1) 
    "additivePersistence 1"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    (0) 
    (0) 
    "additivePersistence 2"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    (-1) 
    (0) 
    "additivePersistence 3"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    (1234567890) 
    (2) 
    "additivePersistence 4"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    (1) 
    (0) 
    "additivePersistence 5"
  ); 

  (fun () -> mkTest 
     digitalRoot 
     (123) 
     (6) 
     "digitalRoot 1"
  );
  (fun () -> mkTest 
     digitalRoot 
     (1) 
     (1) 
     "digitalRoot 2"
  );
  (fun () -> mkTest 
     digitalRoot 
     (-1) 
     (0) 
     "digitalRoot 3"
  );
  (fun () -> mkTest 
     digitalRoot 
     (1234567890) 
     (9) 
     "digitalRoot 4"
  );
  (fun () -> mkTest 
     digitalRoot 
     (0) 
     (0) 
     "digitalRoot 5"
  );
 
  (fun () -> mkTest 
     listReverse
     ([]) 
     ([]) 
     "listReverse 1"
  );
  (fun () -> mkTest 
     listReverse
     ([1;2;3;4;5;6;7;8;9;0]) 
     ([0;9;8;7;6;5;4;3;2;1]) 
     "listReverse 2"
  );
  (fun () -> mkTest 
     listReverse
     ([1]) 
     ([1]) 
     "listReverse 3"
  );
  (fun () -> mkTest 
     listReverse
     (["z";"y";"x"]) 
     (["x";"y";"z"]) 
     "listReverse 4"
  );
  (fun () -> mkTest 
     listReverse
     ([0]) 
     ([0]) 
     "listReverse 5"
  );
  (fun () -> mkTest 
     palindrome
     ("") 
     (true) 
     "palindrome 1"
  );
  (fun () -> mkTest 
     palindrome
     ("kawak") 
     (true) 
     "palindrome 2"
  );
  (fun () -> mkTest 
     palindrome
     ("foo") 
     (false) 
     "palindrome 3"
  );
  (fun () -> mkTest 
     palindrome
     ("a") 
     (true) 
     "palindrome 4"
  );
  (fun () -> mkTest 
     palindrome
     ("ab") 
     (false) 
     "palindrome 5"
  )
  ]

(*130*************************************************************)
(*130**************** END MODIFY *********************************)
(*130*************************************************************)

let doTest f = 
  try f () with ex -> 
    Printf.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
    (Printexc.to_string ex)

let _ =
  let report = List.map doTest (sampleTests @ yourTests) in
  let _ = List.iter print130 (report@([scoreMsg()])) in
  let _ = print130 ("Compiled\n") in
  (!score, !max)

