(*** CS 51 Problem Set 1 ***)
(*** Ryan Wallace ***)
(*** February 12, 2016 ***)

(* Problem 1 - Fill in types:
 *
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(* For each of the expressions below, enter a string describing the 
 * type of the expression in the expressions below. The first one is done 
 * for you.
 *)


let prob0 : int = 42 ;;

let prob1a : string = let greet y = "Hello " ^ y in greet "World!";;

let prob1b : int option list = [Some 4; Some 2; None; Some 3];;

let prob1c : ('a option * float option) * bool = ((None, Some 42.0), true);;


let prob0 =  "int";;

let prob1a = "string";;

let prob1b = "int option list";;

let prob1c = "('a option * float option) * bool";;

(* There are several values defined below that do not type check.

  Explain in a comment above each corresponding value
  why the following definitions will
  not type check, and then provide a fixed version of each function
  as an OCaml value (i.e. outside of a comment). *) 

(*
 * This is a list of string * int tuples, and the provided type
 * is almost correct. However, parentheses are omitted around the 
 * string * int tuple, causing the type to be interpreted as a tuple
 * of a string and an int list, an associativity error. Parentheses
 * around string * int fix the error.

let prob1d : string * int list = [("CS", 51); ("CS", 50)];;
*)

let prob1d : (string * int) list = [("CS", 51); ("CS", 50)];;

(*
 * The int type is correct, as both 4 and 2 are ints. However,
 * the comparison operator < is of type 'a * 'a -> bool, and 4 and
 * 3.9 are not of the same type 'a. Namely, 4 is an int and 3.9 is
 * a float. Comparing identical types fixes the issue. We'll switch
 * to comparison of two ints.

let prob1e : int =
  let compare (x,y) = x < y in
  if compare (4, 3.9) then 4 else 2;;
*)

let prob1e : int =
  let compare (x,y) = x < y in
  if compare (4, 3) then 4 else 2;;
    
(*
 * The central issue here is the type of the second element of the tuple.
 * The second elements are not strings because they are not enclosed in
 * parentheses. They are also not ints because None is not a value of an
 * int. Further, they are not all int options because Some does not prefix
 * every integer. 

 * I believe the intent is to have a list of (string * int option) tuples.
 * Therefore, changing the type to (string * int option) list, and adding
 * Some before each integer fixes the issue.

let prob1f : (string * string) list =
  [("January", None); ("February", 1); ("March", None); ("April", None);
   ("May", None); ("June", 1); ("July", None); ("August", None);
   ("September", 3); ("October", 1); ("November", 2); ("December", 3)] ;;
*)

let prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); ("April", None);
   ("May", None); ("June", Some 1); ("July", None); ("August", None);
   ("September", Some 3); ("October", Some 1); ("November", Some 2);
   ("December", Some 3)] ;;

(* Problem 2 - Write the following functions *)

(* For each subproblem, you must implement a given function, providing
 * appropriate unit tests in the accompanying file pset1_tests.ml. You
 * are provided a high level description as well as a prototype (type signature)
 * of the function you must implement. 
 * Keep in mind the CS51 style guide 
 * and what you've learned so far about efficiency and elegance. *)

(* `reversed lst` should return true if the integers in lst are in
 * decreasing order. The empty list is considered to be reversed. Consecutive
 * elements can be equal in a reversed list. *)

(* Here is its prototype/signature: *)
(* reversed : int list -> bool *)

(* Implement reversed below, and be sure to write tests for it (see 2b for
 * examples of tests). *)

(* Replace the line below with your own definition of `reversed` *)

(* checks if a list of ints is in weakly descending order *)
let rec reversed (lst : int list) : bool =
  match lst with
  | [] -> true
  | h::d::t -> if h >= d then reversed (d::t) else false
  | _ -> true ;;						  

(* merge takes two integer lists, each sorted in increasing order,
 * and returns a single merged list in sorted order. For example:

merge [1;3;5] [2;4;6];;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;3;5] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

*)

(* The type signature for merge is as follows: *)
(* merge : int list -> int list -> int list *)

(* Replace the line below with your own definition of `merge` *)

(* merges two sorted lists into a singe sorted list, as for
 * in merge sort *)
let rec merge (lst1 : int list) (lst2 : int list) : int list =
  match lst1, lst2 with
  | [], [] -> []
  | _, [] -> lst1
  | [], _ -> lst2
  | h1::t1, h2::t2 ->
     if h1 <= h2 then h1::(merge t1 lst2)
     else h2::(merge lst1 t2) ;;

(* unzip should be a function which, given a list of pairs, returns a
 * pair of lists, the first of which contains each first element of
 * each pair, and the second of which contains each second element.
 * The returned lists should have the elements in the order in which
 * they appeared in the input. So, for instance:

unzip [(1,2);(3,4);(5,6)];;
- : int list * int list = ([1;3;5],[2;4;6])

*)


(* The type signature for unzip is as follows: *)
(* unzip : (int * int) list -> int list * int list) *)

(* Replace the line below with your own definition of `unzip` *)

(* Returns a pair of lists given a list of pairs, preserving order *)
let rec unzip (lst : (int * int) list) : int list * int list =
  match lst with
  | [] -> ([],[])
  | (a, b)::t ->
     let lst1, lst2 = unzip t in (a::lst1,b::lst2) ;;
  

(*>* Problem 2d *>*)

(* `variance lst` returns None if lst has fewer than 2 floats, and
 * Some of the variance of the floats in lst otherwise.  Recall that
 * the variance of a sequence of numbers is 1/(n-1) * sum (x_i-m)^2,
 * where a^2 means a squared, and m is the arithmetic mean of the list
 * (sum of list / length of list). For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0];;
- : int option = Some 2.5
variance [1.0];;
- : int option = None

 * Remember to use the floating point version of the arithmetic
 * operators when operating on floats (+. *., etc). The "float"
 * function can cast an int to a float. *)

(* variance : float list -> float option *)

(* Replace the line below with your own definition of `variance` *)

(* two helper functions, sumpow and length are used to maximize generality/
 * extensibility and readability. 
 * sumpow returns the sum of the elements of a list, each raised to a given
 * power *)
let rec sumpow (lst : float list) (pow : float)  : float =
  match lst with
   | [] -> 0.0
   | h::t -> (h ** pow) +. sumpow t pow ;;

(* returns the number of elements of a float list as a float *)
let rec length (lst : float list) : float =
  match lst with
  | [] -> 0.0
  | _::t -> 1.0 +. length t ;;

(* main logic that computes variance of a list of floats using sumpow and 
 * length as helpers, and a formula for variance *)
let variance (lst : float list) : float option =
  match lst with
  | [] -> None
  | _::[] -> None
  | _ -> Some (((sumpow lst 2.0) -. (((sumpow lst 1.0) ** 2.0)
                  /. length lst)) /. (length lst -. 1.0)) ;;

  
(*>* Problem 2e *>*)

(* few_divisors n m should return true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise. Note that this is *not* the
 * same as n having fewer divisors than m:

few_divisors 17 3;;
- : bool = true
few_divisors 4 3;;
- : bool = false
few_divisors 4 4;;
- : bool = true

 * Do not worry about negative integers at all. We will not test
 * your code using negative values for n and m, and do not
 * consider negative integers for divisors (e.g. don't worry about
 * -2 being a divisor of 4) *)

(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)

(* Replace the line below with your own definition of `few_divisors` *)
  
(* returns true if supplied number has fewer than supplied number of divisors.
 * does not handle negative numbers or consider negative divisors *) 
let few_divisors (div : int) (cmp : int) : bool =
  let rec num_divisors (num : int) (den : int) : int =
    if num = den then 1
    else if num mod den = 0 then 1 + num_divisors num (den + 1)
    else num_divisors num (den + 1) in
  if div = 0 then false else
  num_divisors div 1 < cmp ;;								
  

(*>* Problem 2f *>*)

(* `concat_list sep lst` returns one big string with all the string
 * elements of lst concatenated together, but separated by the string
 * sep. Here are some example tests:

concat_list ", " ["Greg"; "Anna"; "David"];;
- : string = "Greg, Anna, David"
concat_list "..." ["Moo"; "Baaa"; "Quack"];;
- : string = "Moo...Baaa...Quack"
concat_list ", " [];;
- : string = ""
concat_list ", " ["Moo"];;
- : string = "Moo"

*)

(* The type signature for concat_list is: *)
(* concat_list : string -> string list -> string *)

(* Replace the line below with your own definition of `concat_list` *)

(* returns single string containing all elements of string list provided
 * concatenated with string separator between each list element *)
let rec concat_list (sep : string) (lst : string list) : string =
  match lst with
  | [] -> "" 
  | [h] -> h
  | h::t -> h ^ sep ^ concat_list sep t ;;

(*>* Problem 2g *>*)

(* One way to compress a list of characters is to use run-length encoding.
 * The basic idea is that whenever we have repeated characters in a list
 * such as ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] we can
 * (sometimes) represent the same information more compactly as a list
 * of pairs like [(5,'a');(3,'b');(1,'c');(4,'d')].  Here, the numbers
 * represent how many times the character is repeated.  For example,
 * the first character in the string is 'a' and it is repeated 5 times,
 * followed by 3 occurrences of the character 'b', followed by one 'c',
 * and finally 4 copies of 'd'.
 *
 * Write a function to_run_length that converts a list of characters into
 * the run-length encoding, and then write a function from_run_length
 * that converts back. Writing both functions will make it easier to
 * test that you've gotten them right. *)

(* The type signatures for to_run_length and from_run_length are: *)
(* to_run_length : char list -> (int * char) list *)
(* from_run_length : (int * char) list -> char list *)

(* Replace the line below with your own definition of `to_run_length` *)

(* performs a run-length encoding on character list, returning list of
 * int * character pairs, with the int indicating the number of given successive
 * characters. decoded by from_run_length *)
let rec to_run_length (lst : char list) : (int * char) list =
  match lst with
  | [] -> []
  | h::_ ->
   let rec count (lstfrag : char list) : int =
     match lstfrag with
     | [] -> 0
     | h::d::t ->
        if h = d then 1 + count (d::t)
        else 1
     | _ -> 1 in
   let rec remove (lstfrag : char list) : char list =
     match lstfrag with
     | h::d::t ->
	if h = d then remove (d::t)
	else d::t
     | _ -> [] in
   (count lst, h) :: to_run_length (remove lst) ;;
   
    

(* Replace the line below with your own definition of `from_run_length` *)

(* decodes a run-length encoding from a list of int * character pairs,
 * reconstructing a list of characters, each repeated successively the number
 * of times given by the associated int. encoded by to_run_length *)
let rec from_run_length (lst : (int * char) list) : char list =
  match lst with
  | [] -> []
  | (a, b)::t ->
     if a > 0 then b :: from_run_length ((a - 1, b)::t)
     else from_run_length t ;;
  

(*>* Problem 3 *>*)

(* Challenge!

 * permutations lst should return a list containing every
 * permutation of lst. For example, one correct answer to
 * permutations [1; 2; 3] is
 * [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]].

 * It doesn't matter what order the permutations appear in the returned list.
 * Note that if the input list is of length n then the answer should be of
 * length n!.

 * Hint:
 * One way to do this is to write an auxiliary function,
 * interleave : int -> int list -> int list list,
 * that yields all interleavings of its first argument into its second:
 * interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ].
 * You may also find occasion for the library functions
 * List.map and List.concat. *)

(* The type signature for permuations is: *)
(* permutations : int list -> int list list *)

(* Replace the line below with your own definition of `permutations` *)
let permutations = (fun _ -> failwith "ImplementMe") ;;