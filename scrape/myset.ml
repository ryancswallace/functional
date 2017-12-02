(**********************************************************************
 * CS51 Problem Set 5, 2016 - Moogle
 * myset.ml: an interface and simple implementation of a set 
 *           abstract datatype
 **********************************************************************)

open Order

(* Definitions for sets. *)

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set in some
   * unspecified order, using the calling convention of fold_left, that
   * is, if the set s contains s1,...,sn, then
   *      fold f u s
   * returns
   *      (f ... (f (f u s1) s2) ... sn)
  *)
  val fold : ('a -> elt -> 'a) -> 'a -> set -> 'a

  (* functions to convert the types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs the tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end

(* Parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> ordering
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end

(* An example implementation of the COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Equal -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Equal -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)
      
  let fold = List.fold_left

  let string_of_elt = C.string_of_t

  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for the ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end



(***********************************************************************)
(*       Section 2: Sets as dictionaries                     *)
(***********************************************************************)

(* TODO: Uncomment the skeleton code for the DictSet module below and
   complete the implementation, making sure that it conforms to the
   appropriate signature. 

   Add appropriate tests for the functor and make sure that your 
   implementation passes the tests. Once you have the DictSet functor 
   working, you can use it instead of the ListSet implementation by 
   updating the definition of the Make functor below. *)
  
(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(struct
    type key = C.t
    type value = C.t
    let compare = C.compare
    let string_of_key = C.string_of_t
    let string_of_value = C.string_of_t
    let gen_key () = C.gen ()
    let gen_key_gt a () = C.gen_gt a ()
    let gen_key_lt a () = C.gen_lt a ()
    let gen_key_random () = C.gen_random ()
    let gen_key_between a b () = C.gen_between a b ()
    let gen_value () = C.gen ()
    let gen_pair () = (gen_key(), gen_value())
  end)

  type elt = D.key

  type set = D.dict

  let empty = D.empty

  let is_empty d = 
    match D.choose d with
    | Some _ -> false
    | None -> true

  let insert a d = D.insert d a a

  let member d a = D.member d a

  let remove a d = D.remove d a

  let singleton a = insert a empty

  let fold f a d = D.foldl (fun a k _ -> f a k) a d

  let union d1 d2 = fold (fun d k -> insert k d) d1 d2

  let intersect d1 d2 = 
    D.foldl (fun a k _ -> if member d2 k then a
      else remove k a) d1 d1

  let choose d = 
    match D.choose d with
    | Some (k, _, d) -> Some (k, d)
    | None -> None

  let string_of_elt = D.string_of_key

  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for the DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)
  
  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_empty () =
    assert (empty = D.empty);
    ()

  let test_is_empty () =
    assert (is_empty empty) ;
    let l1 = generate_random_list 1 in
    let s1 = insert_list empty l1 in
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in
    assert (not (is_empty s1)) ;
    assert (not (is_empty s2)) ;
    ()

  let test_insert () =
    let l1 = generate_random_list 0 in
    let s1 = insert_list empty l1 in
    assert (s1 = empty);
    let l1 = generate_random_list 10 in
    let s1 = insert_list empty l1 in
    List.iter (fun k -> assert(member s1 k)) l1;
    let l1 = generate_random_list 100 in
    let l2 = generate_random_list 20 in
    let s1 = insert_list empty l1 in
    let s12 = insert_list s1 l2 in
    List.iter (fun k -> assert(member s12 k)) l1;
    List.iter (fun k -> assert(member s12 k)) l2;
    ()

  let test_member () =
    let l1 = C.gen_random() in
    assert (not (member empty l1));
    let s1 = insert l1 empty in
    assert (member s1 l1);
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in
    List.iter (fun l -> assert (member s2 l)) l2;
    let s3 = List.fold_right (fun l s2 -> remove l s2) l2 s2 in
    List.iter (fun l -> assert (not (member s3 l))) l2;
    ()

  let test_remove () =
    let s0 = remove (C.gen_random()) empty in
    assert (s0 = empty);
    let l1 = C.gen() in
    let l1g = C.gen_gt l1 () in
    let s1 = insert l1 empty in
    assert ((remove l1 s1) = empty);
    let s1 = insert l1 empty in
    let s2 = insert l1g s1 in
    assert ((remove l1g s2) = s1);
    ()

  let test_singleton () =
    let l1 = C.gen_random() in
    let s1 = insert l1 empty in
    assert (s1 = singleton l1);
    assert (remove l1 s1 = empty);
    ()

  let test_fold () =
    let l1 = generate_random_list 10 in
    let s1 = insert_list empty l1 in
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in
    let s12 = insert_list s1 l2 in
    assert ((fold (fun d _ -> d) s1 s2) = s1);
    assert ((fold (fun d a -> remove a d) s12 s2) = s1); 
    ()
    
  let test_union () =
    let s1 = empty in
    let s2 = empty in
    assert (union s1 s2 == empty);
    let s1 = empty in
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in
    let u = union s1 s2 in
    List.iter (fun l -> assert (member u l)) l2;
    let l1 = generate_random_list 10 in
    let s1 = insert_list empty l1 in
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in
    let u = union s1 s2 in
    List.iter (fun l -> assert (member u l)) l1;
    List.iter (fun l -> assert (member u l)) l2;
    let l1 = generate_random_list 123 in
    let s1 = insert_list empty l1 in
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in
    let u = union s1 s2 in
    List.iter (fun l -> assert (member u l)) l1;
    List.iter (fun l -> assert (member u l)) l2;
    ()

  let test_intersect () =
    let s1 = empty in 
    let s2 = empty in
    assert (intersect s1 s2 = empty);
    let l1 = C.gen_random() in
    let s1 = insert l1 empty in 
    let s2 = empty in
    assert (intersect s1 s2 = empty);
    let s2 = empty in
    let l1 = generate_random_list 10 in
    let s1 = insert_list empty l1 in 
    assert (intersect s1 s2 = empty);
    let s1 = empty in
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in 
    assert (intersect s1 s2 = empty);
    let l1 = C.gen() in
    let l2 = C.gen_gt l1 () in
    let l3 = C.gen_gt l2 () in
    let s1 = insert l1 empty in
    let s2 = insert l2 s1 in
    assert (intersect s1 s2 = s1);
    let s3 = insert l2 empty in
    let s4 = insert l3 s3 in
    assert (intersect s1 s4 = empty);
    ()

  let test_choose () =
    assert (choose empty = None);
    let l1 = C.gen_random() in
    let s1 = insert l1 empty in
    assert (choose s1 = Some (l1, empty));
    let l2 = generate_random_list 10 in
    let s2 = insert_list empty l2 in
    match choose s2 with
      | Some (a, b) -> assert (not (member b a));
                       assert (member s2 a);
      | None -> assert (false);
    ()

  let test_invariant () =
    let l1 = C.gen_random() in
    let s1 = insert l1 empty in
    let s11 = insert l1 s1 in
    assert (s1 = s11);
    let s10 = remove l1 s11 in
    assert (s10 = empty);
    ()

  let run_tests () =
    test_empty () ;
    test_is_empty () ;
    test_insert () ;
    test_member () ;
    test_remove () ;
    test_singleton () ; 
    test_fold () ;
    test_union () ;
    test_intersect () ;
    test_choose () ;
    test_invariant () ;
    ()
end

(******************************************************************)
(* Run the tests.                                                 *)
(******************************************************************)
module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;

(* Create a set of ints using the ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests() ;;
  
(***********************************************************************)
(*    Section 3: Update set implementation and try your crawler        *)
(***********************************************************************)

(******************************************************************)
(* Make: a functor that creates a SET by calling the              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use the dictionary implementation of sets 
     when you are finished. *)
  (* ListSet (C)  *)
  DictSet (C)
  

