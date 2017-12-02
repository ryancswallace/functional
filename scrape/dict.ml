(**********************************************************************
 * CS51 Problem Set 5, 2016 - Moogle
 * dict.ml: an interface and simple implementations of a dictionary 
 *          abstract datatype.
 **********************************************************************)

open Order ;;

(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)
module type DICT =
sig
  type key
  type value
  type dict

  (* An empty dictionary *)
  val empty : dict

  (* Folds the dictionary using the provided function f and base
   * case u using the calling convention of fold_left: 
   *
   *    foldl f u dict
   *
   * The fold function f must have the type:
   *      key -> value -> 'a -> 'a
   * and the base case u has type 'a.
   *
   * If the dictionary has the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold f d u should return:
   *      f (f (... ((f(u k1 v1) k2 v2) k3 v3) ... kn vn)
   *)
  val foldl : ('a -> key -> value -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* various testing functions! *)
  val test_insert : unit -> bool
  val test_remove : unit -> bool
  val test_choose : unit -> bool
  val test_foldl : unit -> bool

  (* Runs all the tests. see TESTING EXPLANATION below *)
  val run_tests : unit -> bool
end


(* Argument module signature to our DICT functors *)
module type DICT_ARG =
  sig
    type key
    type value
    val compare : key -> key -> ordering
    val string_of_key : key -> string
    val string_of_value : value -> string
             
    (* Use these functions for testing. See TESTING EXPLANATION. *)
             
    (* Generate a key. The same key is always returned *)
    val gen_key : unit -> key
          
    (* Generate a random key. *)
    val gen_key_random : unit -> key
           
    (* Generates a key greater than the argument. *)
    val gen_key_gt : key -> unit -> key
              
    (* Generates a key less than the argument. *)
    val gen_key_lt : key -> unit -> key
              
    (* Generates a key between the two arguments. Return None if no such
     * key exists. *)
    val gen_key_between : key -> key -> unit -> key option
                
    (* Generates a random value. *)
    val gen_value : unit -> value
            
    (* Generates a random (key,value) pair *)
    val gen_pair : unit -> key * value
  end

(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : (DICT_ARG with type key = int
        and type value = string) =
  struct
    type key = int
    type value = string
    let compare x y = if x < y then Less else if x > y then Greater else Equal
    let string_of_key = string_of_int
    let string_of_value v = v
    let gen_key () = 0
    let gen_key_gt x () = x + 1
    let gen_key_lt x () = x - 1
    let gen_key_between x y () =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
    let gen_key_random =
      let _ = Random.self_init () in
      (fun () -> Random.int 10000)
  
    (* returns the nth string in lst, or "cow" n > length of list *)
    let rec lst_n (lst: string list) (n: int) : string =
      match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)
                
    (* list of possible values to generate *)
    let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                           "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                           "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
    let num_values = List.length possible_values
    (* gen_value will return the string at this current index *)
    let current_index = ref 0
    let gen_value () =
      let index = !current_index in
      if index >= num_values then
  (current_index := 0; lst_n possible_values index)
      else
  (current_index := index + 1; lst_n possible_values index)
    let gen_pair () = (gen_key_random(), gen_value())
  end
    
    
(* An association list implementation of our DICT signature. *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
            and type value = D.value) =
struct
  type key = D.key;;
  type value = D.value;;
  type dict = (key * value) list;;

  (* INVARIANT: sorted by key, no duplicates *)

  let empty = [] ;;

  let foldl f u d =
    List.fold_left (fun a (k,v) -> f a k v) u d

  let rec lookup d k =
    match d with
      | [] -> None
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Equal -> Some v1
          | Greater -> lookup d1 k
          | _ -> None)

  let member d k =
    match lookup d k with
      | None -> false
      | Some _ -> true

  let rec insert d k v =
    match d with
      | [] -> [(k,v)]
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Less -> (k,v)::d
          | Equal -> (k,v)::d1
          | Greater -> (k1,v1)::(insert d1 k v))

  let rec remove d k =
    match d with
      | [] -> []
      | (k1,v1)::d1 ->
  (match D.compare k k1 with
          | Equal -> d1
          | Greater -> (k1,v1)::(remove d1 k)
          | _ -> d)

  let choose d =
    match d with
      | [] -> None
      | (k,v)::rest -> Some(k,v,rest)

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string =
    let f = (fun y (k,v) -> y ^ "\n key: " ^ D.string_of_key k ^
      "; value: (" ^ D.string_of_value v ^ ")") in
    List.fold_left f "" d

  (****************************************************************)
  (* Tests for our AssocListDict functor                          *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.fold_left (fun hd (k,v) -> hd && (lookup d1 k = Some v)) true pairs1

  let test_remove () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.fold_left
      (fun hd (k,_) ->
        let r = remove d1 k in
        List.fold_left
          (fun _hd2 (k2,v2) ->
            if k = k2 then hd && (lookup r k2 = None)
            else hd && (lookup r k2 = Some v2)
          ) true pairs1
      ) true pairs1


  let test_lookup () =
    true

  let test_choose () =
    true

  let test_member () =
    true

  let test_foldl () =
    true

  let run_tests () =
    test_insert() &&
    test_remove() &&
    test_lookup() &&
    test_choose() &&
    test_member() &&
    test_foldl()

end


(* A binary search tree implementation of our DICT signature. This
   implementation doesn't maintain balance in the trees. *)
module BSTDict(D:DICT_ARG) : (DICT with type key = D.key
            and type value = D.value) =
struct
  type key = D.key;;
  type value = D.value;;
  type dict =
    | E
    | T of dict * (key * value) * dict ;;

  (* INVARIANT: keys in left dict are all less than key; keys in right
     dict are all greater than key; no duplicate keys *)

  let empty = E ;;

  let rec foldl f u d =
    match d with
    | E -> u
    | T(dl, (k,v), dr) ->
       f (foldl f (foldl f u dl) dr) k v ;;

  let rec lookup d k =
    match d with
      | E -> None
      | T(dl, (k1, v1), dr) ->
        match D.compare k k1 with
        | Equal -> Some v1
        | Less -> lookup dl k
        | Greater -> lookup dr k

  let member d k =
    match lookup d k with
      | None -> false
      | Some _ -> true

  let rec insert d k v =
    match d with
      | E -> T(E, (k,v), E)
      | T(dl, ((k1, _v1) as kv), dr) ->
         (match D.compare k k1 with
          | Equal -> T(dl, (k,v), dr)
          | Less -> T(insert dl k v, kv, dr)
          | Greater -> T(dl, kv, insert dr k v))

  let rec max d =
    match d with
    | E -> None
    | T(_dl, _kv, dr) ->
       match dr with
       | E -> Some d
       | _ -> max dr

  exception Dict_internal_error
        
  let rec remove d k =
    match d with
    | E -> E
    | T(dl, ((k1, _v1) as kv), dr) ->
       match D.compare k k1 with
       | Less -> T((remove dl k), kv, dr)
       | Greater -> T(dl, kv, (remove dr k))
       | Equal ->
    match dl, dr with
    | E, E -> E
    | dl, E -> dl
    | E, dr -> dr
    | _dl, _dr ->
       match max dl with
       | None -> raise Dict_internal_error
       | Some E -> raise Dict_internal_error
       | Some T(_, ((km, _vm) as kvm), _) ->
    T((remove dl km), kvm, dr)
    
  let choose d =
    match d with
      | E -> None
      | T(_dl, (k,v), _dr) ->
   Some (k, v, (remove d k))

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string =
    let f = (fun str k v ->
       "key: "
       ^ D.string_of_key k
       ^ "; value: ("
       ^ D.string_of_value v
       ^ ")\n"
       ^ str ) in
    foldl f "end\n" d

  (****************************************************************)
  (* Tests for the functor                                        *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.fold_left (fun hd (k,v) -> hd && (lookup d1 k = Some v)) true pairs1

  let test_remove () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.fold_left
      (fun hd (k,_) ->
        let r = remove d1 k in
        List.fold_left
          (fun _hd2 (k2,v2) ->
            if k = k2 then hd && (lookup r k2 = None)
            else hd && (lookup r k2 = Some v2)
          ) true pairs1
      ) true pairs1


  let test_lookup () =
    true

  let test_choose () =
    true

  let test_member () =
    true

  let test_foldl () =
    true

  let run_tests () =
    test_insert() &&
    test_remove() &&
    test_lookup() &&
    test_choose() &&
    test_member() &&
    test_foldl()
end


(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our
 * AssocListDict functor and run the tests *)
module IntStringListDict = AssocListDict(IntStringDictArg) ;;
IntStringListDict.run_tests();;

(******************************************************************)
(* Make: a functor that creates a DICT by calling our             *)
(* AssocListDict functors                               *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
          with type value = D.value) =
  (* AssocListDict(D) *)
  BSTDict(D)

