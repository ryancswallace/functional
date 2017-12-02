open Ps1

(* tests for each of the functions (in ps1.ml) in comments are included below.
 * some included tests are drawn from examples given in the problem set. *)
       
(* reversed *)
let () = assert ((reversed []) = true);;
let () = assert ((reversed [1]) = true);;
let () = assert ((reversed [-1]) = true);;
let () = assert ((reversed [2;1;0]) = true);;
let () = assert ((reversed [2;2;0]) = true);;
let () = assert ((reversed [-200;-200;-400]) = true);;
let () = assert ((reversed [-200;-200;700]) = false);;
let () = assert ((reversed [200;1;0]) = true);;
let () = assert ((reversed [0;1;2]) = false);;
let () = assert ((reversed [0;1;200]) = false);;
let () = assert ((reversed [2;1;-1]) = true);;
let () = assert ((reversed [200;-200]) = true);;
let () = assert ((reversed [-200;200]) = false);;
let () = assert ((reversed [-1;1;2]) = false);;
let () = assert ((reversed [0;1;-200]) = false);;

(* merge *)
let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
let () = assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
let () = assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-1]) = [-1]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;

(* unzip *)
let () = assert ((unzip []) = ([],[]));;
let () = assert ((unzip [(1,2)]) = ([1],[2]));;
let () = assert ((unzip [(-1,-2)]) = ([-1],[-2]));;
let () = assert ((unzip [(-100,2);(300,-4)]) = ([-100;300],[2;-4]));;
let () = assert ((unzip [(1000,-2);(-300,4);(5,-600)]) = ([1000;-300;5],
							  [-2;4;-600]));;

(* variance *)
let () = assert ((variance []) = None);;
let () = assert ((variance [1.0]) = None);;
let () = assert ((variance [-1.0]) = None);;
let () = assert ((variance [1.0;1.0;1.0]) = Some 0.0);;
let () = assert ((variance [-1.0;1.0]) = Some 2.0);;
let () = assert ((variance [-1.0;1.0;1.0]) = Some (4.0 /. 3.0));;
let () = assert ((variance [-1.0;0.0;1.0]) = Some 1.0);;
let () = assert ((variance [1.0; 2.0; 3.0; 4.0; 5.0]) = Some 2.5);;
let () = assert ((variance [-100.5; 2.3; -30.0; 4.0; 500.8]) =
		   Some (58364167.0 /. 1000.0));;

(* few_divisors *)
let () = assert ((few_divisors 0 0) = false);;
let () = assert ((few_divisors 0 1) = false);;
let () = assert ((few_divisors 0 100) = false);;
let () = assert ((few_divisors 1 0) = false);;
let () = assert ((few_divisors 1 1) = false);;
let () = assert ((few_divisors 1 2) = true);;
let () = assert ((few_divisors 17 3) = true);;
let () = assert ((few_divisors 17 2) = false);;
let () = assert ((few_divisors 100 9) = false);;
let () = assert ((few_divisors 100 10) = true);;
let () = assert ((few_divisors 100 1000) = true);;

(* concat_list *)
let () = assert ((concat_list "" []) = "");;
let () = assert ((concat_list "a7&  .tom brady" []) = "");;
let () = assert ((concat_list "" ["h"]) = "h");;
let () = assert ((concat_list "a7&  .tom brady" ["h"]) = "h");;
let () = assert ((concat_list "" ["6%df"; "asdf"]) = "6%dfasdf");;
let () = assert ((concat_list "" ["6%df"; "asdf"; "$3#h"]) =
		   "6%dfasdf$3#h");;
let () = assert ((concat_list "a7&  .tom brady " ["6%df"; "asdf"]) =
		   "6%dfa7&  .tom brady asdf");;
let () = assert ((concat_list "a7&  .tom brady " ["6%df"; "asdf"; "$3#h"]) =
		   "6%dfa7&  .tom brady asdfa7&  .tom brady $3#h");;

(* to_run_length *)
let () = assert ((to_run_length []) = []);;
let () = assert ((to_run_length ['a']) = [(1,'a')]);;
let () = assert ((to_run_length ['7';'7']) = [(2,'7')]);;
let () = assert ((to_run_length ['$';'$';'$';'$';'$';'$']) = [(6,'$')]);;
let () = assert ((to_run_length ['7';'7';'a';'a']) = [(2,'7');(2,'a')]);;
let () = assert ((to_run_length ['7';'a';'7';'b']) = [(1,'7');(1,'a');
						      (1,'7');(1,'b')]);;
let () = assert ((to_run_length ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';
				 'd';'d';'d']) = [(5,'a');(3,'b');(1,'c');
						  (4,'d')]);;

(* from_run_length *)
let () = assert ((from_run_length []) = []);;
let () = assert ((from_run_length [(1,'a')]) = ['a']);;
let () = assert ((from_run_length [(2,'7')]) = ['7';'7']);;
let () = assert ((from_run_length [(6,'$')]) = ['$';'$';'$';'$';'$';'$']);;
let () = assert ((from_run_length [(2,'7');(2,'a')]) = ['7';'7';'a';'a']);;
let () = assert ((from_run_length [(1,'7');(1,'a');(1,'7');(1,'b')]) =
		   ['7';'a';'7';'b']);;
let () = assert ((from_run_length [(5,'a');(3,'b');(1,'c');(4,'d')]) =
		   ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d']);;

(* composition of to_run_length and from_run_length *)
let () = assert ((from_run_length (to_run_length [])) = []);;
let () = assert ((from_run_length (to_run_length ['5'])) = ['5']);;
let () = assert ((from_run_length (to_run_length ['7';'a';'7';'b';'b'])) =
		   ['7';'a';'7';'b';'b']);;
let () = assert ((to_run_length (from_run_length [])) = []);;
let () = assert ((to_run_length (from_run_length [(6,'$')])) = [(6,'$')]);;
let () = assert ((to_run_length (from_run_length [(5,'a');(1,'b');(1,'c');
			  (4,'d')])) = [(5,'a');(1,'b');(1,'c');(4,'d')]);;
