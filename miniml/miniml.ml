(* MiniML: a subset of ML
   Read-eval-print loop
   Stuart M. Shieber
   November 19, 2015
 *)

open Printf ;;
open Str ;;

(* str_to_exp: string -> expr
   Returns the expression specified by the string using the Miniml
   parser. *)
let str_to_exp (str: string) : Expr.expr =
  let lexbuf = Lexing.from_string str in
  let exp = Miniml_parse.input Miniml_lex.token lexbuf in
  exp
;;

(* Read-eval-print loop for MiniML *)
  
let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Evaluation.Env.create () in
  printf "Entering %s...\n" Sys.argv.(0);
  flush stdout;
  while true do
    (try
        printf "<== ";
  flush stdout;
  (* read and parse an expression from the input *)
        let exp = Miniml_parse.input Miniml_lex.token lexbuf in
        (* evaluate it *)
        let res = Evaluation.evaluate exp env in
        (* print the result *)
  match res with
  | Evaluation.Env.Val res ->  printf "==> %s\n" (Expr.exp_to_string_f res true)
    (* this match case can indeed happen if an un-applied function is written *)
  | Evaluation.Env.Closure(ex, en) -> printf "==> %s\n" 
      (Expr.exp_to_string_f ex true)
      with
      | Evaluation.EvalException -> printf "xx> evaluation exception\n"
      | Evaluation.EvalError message -> printf "%s\n" message
      | Parsing.Parse_error -> printf "xx> parse error\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );
    flush stdout
  done
;;

(* Run repl if called from command line *)

try
  let _ = Str.search_forward (Str.regexp "miniml\\.\\(byte\\|native\\)") 
    (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;
