(**********************************************************************
 * CS51 Problem Set 3
 * Bignums and RSA
 *)

(* ====================================================================
 * Section 1: Bignums
 *)

type bignum = {neg: bool; coeffs: int list} ;;
  
let base = 1000 ;;

(* ....................................................................
 * Basics 
 *)

(* Problem 1 *)
  
let negate (b : bignum) : bignum =
  if b.coeffs != [] then {neg = not b.neg; coeffs = b.coeffs} else b;;
  
(* Problem 2 *)
  
let equal (b1 : bignum) (b2 : bignum) : bool =
  (b1.neg == b2.neg && b1.coeffs = b2.coeffs) ;;

let less (b1 : bignum) (b2 : bignum) : bool =
  if (b1.neg != b2.neg) then b1.neg else
    if (List.length b1.coeffs < List.length b2.coeffs) then not b1.neg
      else if (List.length b1.coeffs > List.length b2.coeffs) then b1.neg
        else let rec cmp (l1 : int list) (l2 : int list) : bool = 
          match l1, l2 with
          | h1::t1, h2::t2 -> if h1 == h2 then (cmp t1 t2) else 
            if b1.neg then (h1 > h2) else (h1 < h2)
          | _, _ -> false 
        in cmp b1.coeffs b2.coeffs ;;

let greater (b1 : bignum) (b2 : bignum) : bool =
   not (less b1 b2) && not (equal b1 b2) ;;

(* Problem 3 *)
  
let fromInt (n : int) : bignum =
  let rec toList (i : int) : int list =
    if i == 0 then [] else (i mod base)::toList (i / base)
  in {neg = (n < 0); coeffs = List.rev (toList (abs n))} ;;

let toInt (b : bignum) : int option =
  if greater {neg = false; coeffs = b.coeffs} (fromInt max_int) then None
  else let rec sum (l : int list) : int = 
    match l with
    | [] -> 0
    | h::t -> (h + base * sum t)
  in if b.neg then Some(-sum (List.rev b.coeffs))
    else Some (sum (List.rev b.coeffs)) ;;
   

(* ....................................................................
 * Some helpful functions 
 *)

(* Removes zero coefficients from the beginning of the coefficients in
 * a bignum representation *)
let rec stripzeroes (b : int list) : int list =
  match b with
  | 0 :: t -> stripzeroes t
  | _ -> b ;;

(* Removes zero coefficients from the beginning of a bignum
 * representation *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = stripzeroes b.coeffs} ;;

(* Returns a random bignum from 0 to bound (inclusive).
 * Can use this to help randomly test functions. *)
let randbignum (bound: bignum) =
  let randbase = List.map (fun _ -> Random.int base) in
  let rec randbignum_rec (bound: int list) =
    match bound with
      | [] -> []
      | h::t -> let r = Random.int (h+1) in
          r::((if r = h then randbignum_rec else randbase) t)
  in {neg = false; coeffs = stripzeroes (randbignum_rec bound.coeffs)} ;;
       
(* Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s 1 (len - 1)) ;;

(* Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
    | [] -> ""
    | c :: t -> String.make 1 c ^ implode t ;;
            
(* Returns the first n elements of list l (or the whole list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
    | [] -> []
    | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1) ;;

(* Returns a pair
 * (first n elements of lst, rest of elements of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h :: t -> let (lst1, lst2) = split t (n - 1) in
                (h :: lst1, lst2) ;;

(* Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  int_of_float (log10 (float_of_int base)) ;;

(* fromString and toString assume the base is a power of 10 *)
(* Converts a string representing an integer to a bignum. *)
let fromString (s : string) : bignum =
  let rec fromString_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: fromString_rec rest
  in
  match explode s with
    | [] -> fromInt 0
    | h :: t -> if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false;
            coeffs = (List.rev (fromString_rec (List.rev (h :: t))))}

(* Converts a bignum to its string representation.
 * Returns a string beginning with ~ for negative integers. *)
let toString (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s else
      "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s 1 (String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
      | [] -> ""
      | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                  ^ coeffs_to_string t in
  let stripped = stripzeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs ;;

(* ....................................................................
 * Arithmetic functions 
 *)

(* Returns a bignum representing b1 + b2.
 * Assumes that b1 + b2 > 0. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
            : bool * int list =
    match (coeffs1, coeffs2) with
      | ([], []) -> pair_from_carry carry
      | ([], _) -> if carry = 0 then (neg2, coeffs2) else
          plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
      | (_, []) -> if carry = 0 then (neg1, coeffs1) else
          plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
      | (h1 :: t1, h2 :: t2) ->
          let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
          let result = h1 * sign1 + h2 * sign2 + carry in
          if result < 0 then
            let (negres, coeffsres) =
                  plus_with_carry (neg1, t1) (neg2, t2) (-1)
            in (negres, result + base :: coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, result - base :: coeffsres)
          else
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
        plus_with_carry (b1.neg, List.rev b1.coeffs)
                        (b2.neg, List.rev b2.coeffs)
                        0
  in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)} ;;

(* Problem 4 *)
       
(* Returns a bignum representing b1 + b2.
 * Does not make the assumption from plus_pos.
 * Hint: How can you use plus_pos to implement this?
*)
let plus (b1 : bignum) (b2 : bignum) : bignum =
  if (b1.neg == b2.neg) then 
    (if b1.neg == false then plus_pos b1 b2
    else negate (plus_pos (negate b1) (negate b2)))
  else if b1.coeffs = b2.coeffs then {neg = false; coeffs = []}
  else if (b1.neg == true && less {neg = false; coeffs = b1.coeffs} b2)
    || (b1.neg == false && greater b1 {neg = false; coeffs = b2.coeffs})
    then (plus_pos b1 b2)
  else negate (plus_pos (negate b1) (negate b2)) ;;

(* Problem 5 *)
  
(* Returns a bignum representing b1*b2.
 * Things to think about:
 * 1. How did you learn to multiply in grade school?
 * (In case you were never taught to multiply and instead started with
 * set theory in third grade, here's an example):
 *
 *      543
 *     x 42
 *     ____
 *     1086
 *   +21720
 *   =22806
 *
 * 2. Can you use any functions you've already written?
 * 3. How can you break this problem into a few simpler, easier-to-implement
 * problems?
 * 4. If your code is buggy, test helper functions individually instead of
 * the entire set at once.
 * 5. Assuming positivity in some of your helper functions is okay to
 * simplify code, as long as you respect that invariant.
*)

(* Multiplies an int list by a single int *)
let rec num_by_int (n : int list) (i : int) (c : int): int list = 
  match n with
  | [] -> if c = 0 then [] else [c]
  | h::t -> 
    let cval = (h * i + c) mod base in 
      let carry = (h * i + c - cval) / base
  in cval :: num_by_int t i carry ;; 

(* Multiplies a bignum by a bignum *)
let times (b1 : bignum) (b2 : bignum) : bignum =
  if (b1.coeffs = [] || b2.coeffs = []) then {neg = false; coeffs = []} else
    let rec list_by_list (l1 : int list) (l2 : int list) : int list = 
    match l2 with
      | [] -> []
      | h::t -> 
        match t with 
        | [] -> List.rev (num_by_int (List.rev l1) h 0)
        | _ -> let sum = plus {neg = false; coeffs = 
                  List.rev (num_by_int (List.rev l1) h 0)}
                {neg = false; coeffs = 
                  List.rev (num_by_int (List.rev (list_by_list l1 t)) base 0)}
              in sum.coeffs
    in {neg = b1.neg <> b2.neg; coeffs = 
      list_by_list b1.coeffs (List.rev b2.coeffs)} ;;

(* Returns a bignum representing b/n, where n is an integer less than base *)
let divsing (b : int list) (n : int) : int list * int =
  let rec divsing_rec (b : int list) (r : int) : int list * int =
    match b with
      | [] -> [], r
      | h :: t ->
          let dividend = r * base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot * n) in
          (quot :: q, r) in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1 :: h2 :: t -> if h1 < n then divsing_rec (h1 * base + h2 ::t) 0
        else divsing_rec b 0 ;;

(* Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let divmod (b1 : bignum) (b2 : bignum): bignum * bignum =
  let rec divmod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m) else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
        | [] -> failwith "Division by zero"
        | ns :: _ -> let (p, _) =
            if ns + 1 = base then
              (take_first mc (List.length mc - List.length nc), 0)
            else
              let den = ns + 1 in
              let num = take_first mc (List.length mc - List.length nc + 1)
              in divsing num den
          in
          let bp = clean {neg = false; coeffs = p} in
          let p2 = clean (if equal bp (fromInt 0) then fromInt 1 else bp) in
            divmod_rec (clean (plus m (negate (times n p2))))
                       (clean n)
                       (clean (plus psum p2))
  in
  divmod_rec (clean b1) (clean b2) (fromInt 0) ;;

(* ====================================================================
 * Section 2: Challenge Problem: The RSA Cryptosystem
 *)

(* A reminder: Challenge problems are for your karmic edification
 * only. You should feel free to do these after you've done your best
 * work on the primary part of the problem set. *)

(** Support code for RSA **)
(* Hint: each part of this problem can be implemented in approximately one
 * line of code. *)

(* Returns b to the power of e mod m *)
let rec expmod (b : bignum) (e : bignum) (m : bignum) : bignum =
  if equal e (fromInt 0) then fromInt 1
  else if equal e (fromInt 1) then
    snd (divmod (clean b) (clean m))
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = expmod (clean b) q (clean m) in
    let (_, x) = divmod (times (times res res) (expmod (clean b) r (clean m)))
                        (clean m) in
    {neg = x.neg; coeffs = stripzeroes x.coeffs} ;;

(* Returns b to the power of e *)
let rec exponent (b : bignum) (e : bignum) : bignum =
  if equal (clean e) (fromInt 0) then fromInt 1
  else if equal (clean e) (fromInt 1) then clean b
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = exponent (clean b) q in
    let exp = (times (times res res) (exponent (clean b) r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs} ;;

(* Returns true if n is prime, false otherwise. *)
let isPrime (n : bignum) : bool =
  let rec miller_rabin (k : int) (d : bignum) (s : int) : bool =
    if k < 0 then true else
    let rec square (r : int) (x : bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) n in

        if equal x (fromInt 1) then false
        else if equal x (plus n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if equal x (fromInt 1) || equal x (plus n (fromInt (-1))) then
        miller_rabin (k - 1) d s
      else square 1 x 
  in
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n : bignum) (s : int) =
    let (q, r) = divmod n (fromInt 2) in
      if equal r (fromInt 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = divmod n (fromInt 2) in
    if equal r (fromInt 0) then false else
      let (d, s) = factor (plus n (fromInt (-1))) 0 in
        miller_rabin 20 d s ;;

(* Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m : bignum) (d : bignum) : bignum * bignum * bignum =
  if equal d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod m d in
    let (s, t, g) = euclid d r in
      (clean t, clean (plus s (negate (times q t))), clean g) ;;


(* Generate a random prime number between min and max-1 (inclusive) *)
let rec generateRandomPrime (min : bignum) (max: bignum) : bignum =
  let rand = plus (randbignum (plus max (negate min))) min in
    if isPrime rand then rand else generateRandomPrime min max ;;


(** Code for encrypting and decrypting messages using RSA **)

(* Generate a random RSA key pair, returned as (e, d, n).
 * p and q will be between 2^n and 2^(n+1).
 * Recall that (n, e) is the public key, and (n, d) is the private key. *)
let rec generateKeyPair (r : bignum) : bignum * bignum * bignum =
  let c1 = fromInt 1 in
  let c2 = fromInt 2 in
  let p = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  let rec selectPair () =
    let e = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (clean e, clean d, clean (times p q))
      else selectPair ()
  in
    if equal p q then generateKeyPair r else selectPair () ;;


(* Challenge Problem 6 *)

(* To encrypt, pass in n e s. To decrypt, pass in n d s. *)
let encryptDecryptBignum (n : bignum) (e : bignum) (s : bignum) : bignum =
  failwith "encryptDecryptBignum not implemented" ;;


(* Pack a list of chars as a list of bignums, with m chars to a bignum. *)
let rec charsToBignums (lst : char list) (m : int) : bignum list =
  let rec encchars lst =
    match lst with
      | [] -> (fromInt 0)
      | c :: t -> clean (plus (times (encchars t) (fromInt 256))
                              (fromInt (int_of_char c)))
  in
    match lst with
      | [] -> []
      | _ -> let (enclist, rest) = split lst m in
             encchars enclist :: charsToBignums rest m


(* Unpack a list of bignums into chars (reverse of charsToBignums) *)
let rec bignumsToChars (lst : bignum list) : char list =
  let rec decbignum b =
    if equal b (fromInt 0) then []
    else let (q, r) = divmod b (fromInt 256) in
      match toInt r with
        | None -> failwith "bignumsToChars: representation invariant broken"
        | Some ir -> char_of_int ir :: decbignum q
  in
    match lst with
      | [] -> []
      | b :: t -> decbignum b @ bignumsToChars t


(* Return the number of bytes required to represent an RSA modulus. *)
let bytesInKey (n : bignum) =
  int_of_float (float_of_int (List.length (stripzeroes n.coeffs) - 1)
                *. log10 (float_of_int base) /. (log10 2. *. 8.))


(* Encrypts or decrypts a list of bignums using RSA.
 * To encrypt, pass in n e lst.
 * To decrypt, pass in n d lst. *)
let rec encDecBignumList (n : bignum) (e : bignum) (lst : bignum list) =
  match lst with
    | [] -> []
    | h :: t -> encryptDecryptBignum n e h :: encDecBignumList n e t

                     
(* Challenge Problem 7 *)
let encrypt (n : bignum) (e : bignum) (s : string) =
  failwith "encrypt not implemented" ;;

  
(* Decrypt an encrypted message (list of bignums) to produce the
 * original string. *)
let decrypt (n : bignum) (d : bignum) (m : bignum list) =
  failwith "decrypt not implemented" ;;

(* ====================================================================
 * Section 3: Challenge Problem: Faster Multiplication
 *)

(* Challenge Problem 8 *)
  
(* Returns a bignum representing b1*b2 *)
let times_faster (b1 : bignum) (b2 : bignum) : bignum =
  failwith "times_faster not implemented" ;;

(* ====================================================================
 * Section 4: Submit
 *)

let minutes_spent () = 600 ;;
