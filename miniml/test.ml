(* tests for functions in expr.ml and evaluation.ml.
 * testing infrastructure adopted/copied from Prof. Stuart Shieber
 * in Lecture 13, CS51, Spring 2016 *)

open Printf ;;
open Evaluation ;;
open Expr ;;

exception Timeout ;;

type test = 
  {label : string;
   content : bool Lazy.t;
   time : int;
   fail_msg : string} 
;;

type status = 
  | Passed
  | Failed of string
  | Raised_exn of string
  | Timed_out of int
;;

let sigalarm_handler = 
  Sys.Signal_handle (fun _ -> raise Timeout)
;;

let timeout (time : int) (delayed : 'a Lazy.t) : 'a = 
  let old_behavior = 
    Sys.signal Sys.sigalrm sigalarm_handler in
      let reset_sigalarm () = 
        ignore (Unix.alarm 0) ;
        Sys.set_signal Sys.sigalrm old_behavior in
          ignore (Unix.alarm time) ;
          let res = Lazy.force delayed in
          reset_sigalarm () ;
          res 
;;

let run_test ({label; time; content; fail_msg} : test)
             (continue : string -> status -> unit) 
            : unit = 
  try 
    if timeout time content
    then continue label Passed
    else continue label (Failed fail_msg)
  with 
    | Timeout -> continue label (Timed_out time)
    | exn -> continue label (Raised_exn (Printexc.to_string exn))
;;

let present label status = 
  match status with
  | Passed -> printf "%s: passed\n" label
  | Failed msg -> printf "%s: failed %s\n" label msg
  | Timed_out secs -> printf "%s: timed out in %d\n" label secs
  | Raised_exn msg -> printf "%s raised %s\n" label msg
;;

let test ?(fail_msg = "") ?(time = 5) label content = 
  {label = label;
   content = content;
   fail_msg = fail_msg;
   time = time}
;;

let test_eval (m : scope) (exp : expr) : expr =
  let funct = 
    match m with 
    | S -> eval_s
    | D -> eval_d
    | L -> eval_l
  in
  match funct exp (Env.create ()) with
  | Env.Val e -> e
  | Env.Closure (e, env) -> e
;;

let test_expr = 
  [ (* exp_to_string *)
    test "var to string"        
    (lazy (exp_to_string (Var "x") = "Var(x)")) ;
    test "num to string"        
    (lazy (exp_to_string (Num 2) = "Num(2)")) ;
    test "bool to string"       
    (lazy (exp_to_string (Bool true) = "Bool(true)")) ;
    test "unop to string"       
    (lazy (exp_to_string (Unop ("~", Num (2))) = "Unop(~, Num(2))")) ;
    test "binop to string"      
    (lazy (exp_to_string (Binop ("+", Num (3), Num (2))) = "Binop(+, Num(3), " ^
      "Num(2))")) ;
    test "cond to string"       
    (lazy (exp_to_string (Conditional (Bool true, Var "x", Var "y")) = 
      "Conditional(Bool(true), Var(x), Var(y))")) ;
    test "fun to string"        
    (lazy (exp_to_string (Fun ("z", Var "y")) = "Fun(z, Var(y))")) ;
    test "let to string"        
    (lazy (exp_to_string (Let ("f", Fun ("f", Var "z"), Binop ("+", Var "z",
      Num (1)))) = "Let(f, Fun(f, Var(z)), Binop(+, Var(z), Num(1)))")) ;
    test "letrec to string"     
    (lazy (exp_to_string (Letrec ("f", Fun ("f", Var "z"), Binop ("+", 
      Var "z", Num (1)))) = "Letrec(f, Fun(f, Var(z)), Binop(+, Var(z), " ^
      "Num(1)))")) ;
    test "raise to string - should fail with \"Exception raised\""      
    (lazy (exp_to_string Raise = "should fail")) ;
    test "unassigned to string" 
    (lazy (exp_to_string Unassigned = "Unassigned")) ;
    test "app to string"        
    (lazy (exp_to_string (App (Fun ("f", Var "p"), Num(7))) = "App(Fun(f, " ^
      "Var(p)), Num(7))")) ;
    test "big function to string"
    (lazy (exp_to_string (Let ("f", Fun ("x", Var "x"), App (App (Var "f", 
      Var "f"), Num(3)))) = "Let(f, Fun(x, Var(x)), App(App(Var(f), " ^
      "Var(f)), Num(3)))")) ;

    (* free_vars *)
    test "single var"
    (lazy (same_vars (free_vars (Var "x")) (vars_of_list ["x"]))) ;
    test "num"
    (lazy (same_vars (free_vars (Num 5)) (vars_of_list []))) ;
    test "bool"
    (lazy (same_vars (free_vars (Bool false)) (vars_of_list []))) ;
    test "raise - should fail with \"Exception raised\""      
    (lazy (same_vars (free_vars Raise) (vars_of_list []))) ;
    test "unassigned - should fail with \"Unassigned\"" 
    (lazy (same_vars (free_vars Unassigned) (vars_of_list []))) ;
    test "unop w/o free"
    (lazy (same_vars (free_vars (Unop ("~", Num (2)))) (vars_of_list []))) ;
    test "unop w/ 1 free"
    (lazy (same_vars (free_vars (Unop ("~", Var "abc"))) (vars_of_list 
      ["abc"]))) ;
    test "binop w/o free"
    (lazy (same_vars (free_vars (Binop ("/", Num (2), Num (10)))) 
      (vars_of_list []))) ;
    test "binop w/ 1 free"
    (lazy (same_vars (free_vars (Binop ("/", Var "cow", Num (10)))) 
      (vars_of_list ["cow"]))) ;
    test "binop w/ 1 free"
    (lazy (same_vars (free_vars (Binop ("/", Num (5), Var "dog"))) 
      (vars_of_list ["dog"]))) ;
    test "binop w/ 2 free"
    (lazy (same_vars (free_vars (Binop ("/", Var "xyz", Var "abc"))) 
      (vars_of_list ["abc"; "xyz"]))) ;
    test "fun w/o free"
    (lazy (same_vars (free_vars (Fun ("z", Var "z"))) (vars_of_list []))) ;
    test "fun w/0 free"
    (lazy (same_vars (free_vars (Fun ("z", Num(10)))) (vars_of_list []))) ;
    test "fun w/ 1 free"
    (lazy (same_vars (free_vars (Fun ("z", Binop("+", Var "z", Var "y")))) 
      (vars_of_list ["y"]))) ;
    test "fun w/ 2 free"
    (lazy (same_vars (free_vars (Fun ("z", Binop("/", Var "a", Var "b")))) 
      (vars_of_list ["a"; "b"]))) ;
    test "let w/ 0 free"
    (lazy (same_vars (free_vars (Let ("v", Num(2), Binop("*", Var "v", 
      Num(10))))) (vars_of_list []))) ;
    test "let w/ 1 free"
    (lazy (same_vars (free_vars (Let ("v", Var "a", Binop("*", Var "v", 
      Num(10))))) (vars_of_list ["a"]))) ;
    test "letrec w/ 0 free"
    (lazy (same_vars (free_vars (Letrec ("v", Num(2), Binop("*", Var "v", 
      Num(10))))) (vars_of_list []))) ;
    test "letrec w/ 1 free"
    (lazy (same_vars (free_vars (Letrec ("v", Var "a", Binop("*", Var "v", 
      Num(10))))) (vars_of_list ["a"]))) ;
    test "conditional w/o free"
    (lazy (same_vars (free_vars (Conditional (Bool true, Num (2), 
      Num (10)))) (vars_of_list []))) ;
    test "conditional w/ 1 free"
    (lazy (same_vars (free_vars (Conditional (Bool false, Var "x", 
      Num (10)))) (vars_of_list ["x"]))) ;
    test "conditional w/ 2 free"
    (lazy (same_vars (free_vars (Conditional (Bool true, Var "x", Var "y")))
     (vars_of_list ["x"; "y"]))) ;
    test "application w/o free"
    (lazy (same_vars (free_vars (App (Fun ("z", Num(10)), Num (4)))) 
      (vars_of_list []))) ;
    test "application w/ 1 free"
    (lazy (same_vars (free_vars (App (Fun ("z", Var "a"), Num (4)))) 
      (vars_of_list ["a"]))) ;

    (* subst *)
    test "var sub"        
    (lazy (exp_to_string (subst "x" (Var "y") (Var "x")) = exp_to_string 
      (Var "y"))) ;
    test "num sub"        
    (lazy (exp_to_string (subst "x" (Var "y") (Num (5))) = exp_to_string 
      (Num (5)))) ;
    test "bool sub"       
    (lazy (exp_to_string (subst "x" (Var "y") (Bool (false))) = 
      exp_to_string (Bool (false)))) ;        
    test "unop sub"       
    (lazy (exp_to_string (subst "x" (Var "y") (Unop("~", Var "x"))) = 
      exp_to_string (Unop("~", Var "y")))) ;
    test "binop w/o sub"      
    (lazy (exp_to_string (subst "x" (Var "y") (Binop("+", Var "a", 
      Var "z"))) = exp_to_string (Binop("+", Var "a", Var "z")))) ;
    test "binop w/ 1 sub"     
    (lazy (exp_to_string (subst "x" (Var "y") (Binop("+", Var "x", 
      Var "z"))) = exp_to_string (Binop("+", Var "y", Var "z")))) ;
    test "binop w/ 2 sub"     
    (lazy (exp_to_string (subst "x" (Var "y") (Binop("+", Var "x", 
      Var "x"))) = exp_to_string (Binop("+", Var "y", Var "y")))) ;
    test "cond w/o sub"     
    (lazy (exp_to_string (subst "x" (Var "y") (Conditional (Bool true, 
      Var "a", Var "z"))) = exp_to_string (Conditional (Bool true, 
      Var "a", Var "z")))) ;  
    test "cond w/ 1 sub"      
    (lazy (exp_to_string (subst "x" (Var "y") (Conditional (Bool true, 
      Var "x", Var "z"))) = exp_to_string (Conditional (Bool true, 
      Var "y", Var "z")))) ;
    test "fun w/o sub"    
    (lazy (exp_to_string (subst "q" (Var "y") (Fun ("x", Var "a"))) = 
      exp_to_string (Fun ("x", Var "a")))) ;  
    test "fun w/ var sub"   
    (lazy (exp_to_string (subst "x" (Var "y") (Fun ("x", Var "a"))) = 
      exp_to_string (Fun ("x", Var "a")))) ;
    test "fun w/ exp sub"   
    (lazy (exp_to_string (subst "a" (Var "y") (Fun ("x", Var "a"))) = 
      exp_to_string (Fun ("x", Var "y")))) ;    
    test "let w/ param sub. should not sub"
    (lazy (exp_to_string (subst "f" (Var "y") (Let ("f", Fun ("f", Var "z"),
      Binop ("+", Var "z", Num (1))))) = exp_to_string (Let ("f", Fun 
      ("f", Var "z"), Binop ("+", Var "z", Num (1)))))) ;                 
    test "let w/ body sub. not repeated"
    (lazy (exp_to_string (subst "x" (Var "p") (Let ("y", Fun ("f", Var "x"),
      Binop ("+", Var "x", Num (1))))) = exp_to_string (Let ("y", Fun 
      ("f", Var "p"), Binop ("+", Var "p", Num (1)))))) ;
    test "let w/ body sub. repeated - needs replacement"
    (lazy (exp_to_string (subst "x" (Var "y") (Let ("y", Fun ("f", Binop 
      ("+", Var "y", Var "x")), Binop ("+", Var "x", Num (1))))) = 
      exp_to_string (Let ("var0", Fun ("f", Binop ("+", Var "var0", 
      Var "y")), Binop ("+", Var "y", Num (1)))))) ;
    test "letrec w/ param sub. should not sub"
    (lazy (exp_to_string (subst "f" (Var "y") (Letrec ("f", Fun ("f", Var "z"),
      Binop ("+", Var "z", Num (1))))) = exp_to_string (Letrec ("f", Fun 
      ("f", Var "z"), Binop ("+", Var "z", Num (1)))))) ;
    test "letrec w/ body sub. not repeated"
    (lazy (exp_to_string (subst "x" (Var "p") (Letrec ("y", Fun ("f", Var "x"),
      Binop ("+", Var "x", Num (1))))) = exp_to_string (Letrec ("y", Fun 
      ("f", Var "p"), Binop ("+", Var "p", Num (1)))))) ;
    test "letrec w/ body sub. repeated - needs replacement"
    (lazy (exp_to_string (subst "x" (Var "y") (Letrec ("y", Fun ("f", Binop 
      ("+", Var "y", Var "x")), Binop ("+", Var "x", Num (1))))) = 
      exp_to_string (Letrec ("y", Fun ("f", Binop ("+", Var "y", 
      Var "y")), Binop ("+", Var "y", Num (1)))))) ;
    test "letrec in recursive function"
    (lazy (exp_to_string (subst "p" (Var "z") (Letrec ("f", Fun ("m", 
      Conditional (Binop ("=", Var "x", Num (0)), Var "p", App (Var "f", Binop 
      ("-", Var "x", Num (1))))), App (Var "f", Num (0))))) = 
      (exp_to_string (Letrec ("f", Fun ("m", Conditional (Binop ("=", Var "x", 
      Num (0)), Var "z", App (Var "f", Binop ("-", Var "x", Num (1))))), App 
      (Var "f", Num (0))))))) ;
    test "raise - should fail with \"Exception raised\""      
    (lazy (exp_to_string (subst "x" (Var "y") Raise) = exp_to_string 
      (Var "Should Raise"))) ;
    test "unassigned" 
    (lazy (exp_to_string (subst "x" (Var "y") Unassigned) = "Unassigned")) ;
    test "app sub in first arg" 
    (lazy (exp_to_string (subst "x" (Var "y") (App(Fun("f", Var "x"),
      Num(7)))) = exp_to_string (App(Fun("f", Var "y"), Num(7))))) ;        
    test "app sub in second arg"
    (lazy (exp_to_string (subst "x" (Var "y") (App(Fun("f", Var "a"), 
      Var "x"))) = exp_to_string (App(Fun("f", Var "a"), Var "y")))) ;

    (* new_varname *)
    test "new_varname 1 (already called once in subst tests)"
    (lazy (new_varname () = "var1")) ;
    test "new_varname 2"
    (lazy (new_varname () = "var2")) ;
  ]
;;

  let test_env = 
  [ (* env_to_string and value_to_string are extensively tested implicitly in
   the tests below *)
    (* close *)
    test "close - empty environment" 
    (lazy (Env.value_to_string (Env.close (Var "x") (Env.create ())) = 
      "(Var(x), )")) ;
    test "close - non-empty environment"
    (lazy (Env.value_to_string (Env.close (Fun ("z", Var "y")) (Env.extend 
      (Env.create ()) ("x") (ref (Env.Val (Num (10)))))) = "(Fun(z, " ^
      "Var(y)), (x, Num(10)))")) ; 

    (* lookup *)
    test "lookup - empty environment"
    (lazy (Env.value_to_string (Env.lookup (Env.create ()) "x") = 
      "Unassigned")) ;
    test "lookup - 1 elt, present"
    (lazy (Env.value_to_string (Env.lookup (Env.extend (Env.create ()) ("x")
      (ref (Env.Val (Num (10))))) "x") = "Num(10)")) ; 
    test "lookup - 1 elt, not present"
    (lazy (Env.value_to_string (Env.lookup (Env.extend (Env.create ()) ("x") 
      (ref (Env.Val (Num (10))))) "y") = "Unassigned")) ;
    test "lookup - 2 elts, present"
    (lazy (Env.value_to_string (Env.lookup (Env.extend (Env.extend 
      (Env.create ()) ("x") (ref (Env.Val (Num (10))))) ("y") (ref 
      (Env.Val (Bool (true))))) "y") = "Bool(true)")) ;
    test "lookup - 2 elts, not present"
    (lazy (Env.value_to_string (Env.lookup (Env.extend (Env.extend 
      (Env.create ()) ("x") (ref (Env.Val (Num (10))))) ("y") (ref 
      (Env.Val (Bool (true))))) "z") = "Unassigned")) ;
      test "lookup - 3 elts, present, long"
      (lazy (Env.value_to_string (Env.lookup (Env.extend (Env.extend 
        (Env.extend (Env.create ()) ("x") (ref (Env.Val (Num (10))))) ("y")
        (ref (Env.Val (Bool (false))))) ("z") (ref (Env.Val (Let ("f", Fun 
        ("f", Var "z"), Binop ("+", Var "z", Num (1))))))) "z") = "Let(f, " ^
        "Fun(f, Var(z)), Binop(+, Var(z), Num(1)))")) ;
    
    (* extend *)
    test "extend - empty to 1"
    (lazy (Env.env_to_string (Env.extend (Env.create ()) ("x") (ref (Env.Val
      (Num (10))))) = "(x, Num(10))")) ; 
    test "extend - 1 to 2"
    (lazy (Env.env_to_string (Env.extend (Env.extend (Env.create ()) ("x") 
      (ref (Env.Val (Num (10))))) ("y") (ref (Env.Val (Bool (false))))) = 
      "(x, Num(10)), (y, Bool(false))")) ; 
    test "extend - 2 to 3 w/ long exp"
    (lazy (Env.env_to_string (Env.extend (Env.extend (Env.extend 
      (Env.create ()) ("x") (ref (Env.Val (Num (10))))) ("y") (ref 
      (Env.Val (Bool (false))))) ("z") (ref (Env.Val (Let ("f", Fun ("f",
      Var "z"), Binop ("+", Var "z", Num (1))))))) = "(x, Num(10)), (y, " ^
      "Bool(false)), (z, Let(f, Fun(f, Var(z)), Binop(+, Var(z), " ^ 
      "Num(1))))")) ; 
    test "extend - 1 w/ replacement"
    (lazy (Env.env_to_string (Env.extend (Env.extend (Env.create ()) ("x") 
      (ref (Env.Val (Num (10))))) ("x") (ref (Env.Val (Bool (false))))) = 
      "(x, Bool(false))")) ; 
  ]
;;

  let test_evaluate m =
    let str = 
      match m with 
      | S -> "SUBSTITUTION MODEL"
      | D -> "DYNAMIC"
      | L -> "LEXICAL"
    in print_string ("TESTING " ^ str ^ "\n") ;
  [ (* eval *)
    test "var eval - should fail with Unbound x"        
    (lazy (exp_to_string (test_eval m (Var "x")) = "Unbound x")) ;
    test "num eval"       
    (lazy (exp_to_string (test_eval m (Num (10))) = "Num(10)")) ;
    test "bool eval"        
    (lazy (exp_to_string (test_eval m (Bool (true))) = "Bool(true)")) ;
    test "unop eval"      
    (lazy (exp_to_string (test_eval m (Unop ("~", Num (-5)))) = "Num(5)")) ;
    test "binop eval add nums"    
    (lazy (exp_to_string (test_eval m (Binop ("+", Num (3), Num (2)))) = 
      "Num(5)")) ;
    test "binop eval add unops"   
    (lazy (exp_to_string (test_eval m (Binop ("+", Unop ("~", Num (-4)), 
      Unop ("~", Num (6))))) = "Num(-2)")) ;
    test "binop eval add bools - should fail with invalid operands"   
    (lazy (exp_to_string (test_eval m (Binop ("+", Bool (true),
      Num (2)))) = "invalid")) ;    
    test "cond eval true" 
    (lazy (exp_to_string (test_eval m (Conditional (Bool true, Binop ("+", 
      Num (3), Num (2)), Binop ("-", Num (3), Num (2))))) = "Num(5)")) ;      
    test "cond eval false"  
    (lazy (exp_to_string (test_eval m (Conditional (Bool false, Binop ("+",
      Num (3), Num (2)), Binop ("-", Num (3), Num (2))))) = "Num(1)")) ;      
    test "fun eval"       
    (lazy (exp_to_string (test_eval m (Fun ("z", Var "y"))) = 
      "Fun(z, Var(y))")) ;
    test "let eval/app simple"        
    (lazy (exp_to_string (test_eval m (Let ("f", Fun ("x", Num (2)), App 
      (Var "f", Num (1))))) = "Num(2)")) ;
    test "let eval/app double eval"         
    (lazy (exp_to_string (test_eval m (Let ("f", Fun ("x", Var "x"), App 
      (Var "f", Num (1))))) = "Num(1)")) ;
    test "let eval/app triple eval"         
    (lazy (exp_to_string (test_eval m (Let ("f", Fun ("x", Binop ("+", 
      Num (1), Var "x")), App (Var "f", Unop ("~", Num (10)))))) = 
      "Num(-9)")) ;
    test "let eval/app scope matters"         
    (lazy (exp_to_string (test_eval m (Let ("x", Num (1), Let ("f", Fun 
      ("y", Binop ("+", Var "x", Var "y")), Let ("x", Num (2), App 
      (Var "f", Num (3))))))) = if m = S || m = L then "Num(4)" else 
      "Num(5)")) ;
    test "letrec eval very simple"
    (lazy (exp_to_string (test_eval m (Letrec ("f", Fun ("x", Conditional 
      (Binop ("=", Var "x", Num (0)), Num(0), App (Var "f", Binop ("-", 
      Var "x", Num (1))))), App (Var "f", Num (0))))) = "Num(0)")) ;
    test "letrec eval simple"
    (lazy (exp_to_string (test_eval m (Letrec ("f", Fun ("x", Conditional 
      (Binop ("=", Var "x", Num (0)), Var "x", App (Var "f", Binop ("-", 
      Var "x", Num (1))))), App (Var "f", Num (4))))) = "Num(0)")) ;
    test "letrec eval"      
    (lazy (exp_to_string (test_eval m (Letrec ("f", Fun ("x", Conditional 
      (Binop ("=", Var "x", Num (0)), Num (1), Binop ("*", Var "x", App 
      (Var "f", Binop ("-", Var "x", Num (1)))))), App (Var "f", 
      Num (4))))) = "Num(24)")) ;
    test "raise eval - should fail with \"EvalException\""      
    (lazy (exp_to_string (test_eval m (Raise)) = "Raise")) ;
    test "unassigned eval - should fail with \"EvalError - 
      Unassigned Variable\"" 
    (lazy (exp_to_string (test_eval m (Unassigned)) = "Unassigned")) ;
  ]
;;

let report (tests : test list) : unit = 
  List.iter (fun test -> run_test test present) tests
;;

report test_env;
report test_expr; 
report (test_evaluate S);
report (test_evaluate D); 
report (test_evaluate L);
