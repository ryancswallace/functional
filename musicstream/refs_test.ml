(**********************************************************************
              CS51 Problem Set 6
                 Spring 2016
               Refs, Streams, and Music

             Part 1: Refs Testing
 **********************************************************************)

(* Make your refs solution available for testing *)
open Refs ;;

let test () = 
  (* Establish some mutable lists for testing. *)
  let l1 = Cons(1, ref Nil) in
  let l2 = Cons(1, ref l1) in
  let l3 = Cons(1, ref l2) in
  let rec l4 = Cons(1, ref l4) in
  let rec l5 = Cons(1, ref (Cons(2, ref l5))) in

  (* Some example tests. You'll want more. *)
  assert(not(has_cycle l1));
  assert(has_cycle l4);

  (* has_cycle *)
  assert (not(has_cycle l1));
  assert (not(has_cycle l2));
  assert (not(has_cycle l3));
  assert (has_cycle l4);
  assert (has_cycle l5);

  (* mlength *)
  assert ((mlength l1) = 1);
  assert ((mlength l2) = 2);
  assert ((mlength l3) = 3);
  assert ((mlength l4) = 1);
  assert ((mlength l5) = 2);

  (* flatten *)
  let _ = flatten l4 in
  let _ = flatten l5 in
  assert (not(has_cycle l4));
  assert (not(has_cycle l5));
;;

test();;

