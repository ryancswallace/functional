
open Ps3 ;;
  
(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)
let _ = assert(negate {neg = false; coeffs = []}
                    = {neg = false; coeffs = []})
let _ = assert(negate {neg = true; coeffs = [1; 2]}
                    = {neg = false; coeffs = [1; 2]})
let _ = assert(negate {neg = false; coeffs = [10;001]}
                    = {neg = true; coeffs = [10;001]})

(* we test fromInt and toInt here, out of order becuase other tests 
 * rely on them *)

(* fromInt*)
let _ = assert(fromInt (-12345678) = {neg = true; coeffs = [12;345;678]})
let _ = assert(fromInt (-12345) = {neg = true; coeffs = [12;345]})
let _ = assert(fromInt (-1234) = {neg = true; coeffs = [1;234]})
let _ = assert(fromInt (-123) = {neg = true; coeffs = [123]})
let _ = assert(fromInt (-1) = {neg = true; coeffs = [1]})
let _ = assert(fromInt (0) = {neg = false; coeffs = []})
let _ = assert(fromInt (1) = {neg = false; coeffs = [1]})
let _ = assert(fromInt (123) = {neg = false; coeffs = [123]})
let _ = assert(fromInt (1234) = {neg = false; coeffs = [1;234]})
let _ = assert(fromInt (12345) = {neg = false; coeffs = [12;345]})
let _ = assert(fromInt (12345678) = {neg = false; coeffs = [12;345;678]})

(* toInt *)
let _ = assert(toInt {neg = false; coeffs = [46;116;860;184;273;877;904]} = 
  None)
let _ = assert(toInt {neg = true; coeffs = [46;116;860;184;273;877;904]} = 
  None)
let _ = assert(toInt (fromInt 4611686018427387903) = Some 4611686018427387903)
let _ = assert(toInt (fromInt (-4611686018427387903)) =
   Some (-4611686018427387903))
let _ = assert(toInt (fromInt 90210) = Some 90210)
let _ = assert(toInt (fromInt (-90210)) = Some (-90210))
let _ = assert(toInt (fromInt 1) = Some 1)
let _ = assert(toInt (fromInt (-1)) = Some (-1))
let _ = assert(toInt (fromInt 0) = Some 0)
let _ = assert(toInt (fromInt (-0)) = Some 0)


          
(* Some advice on automated testing:

   Here is an automated testing function that checks every pair of
   integers between count and max to verify that the bignum
   representations of count and max are the same if and only if count
   and max are.

   Use this function to help you catch potential edge cases. While
   this kind of automated testing is helpful, it is still important
   for you to think about what cases may be difficult for your
   algorithm. Also, think about what inputs to run the testing
   function on. If you're having trouble isolating a bug, you can try
   printing out which values cause an assert failure.

   You may find that adding this sort of testing function for other
   functions is useful.  *)

(* equal *)       
let rec test_equal (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(equal (fromInt count) (fromInt max) = (count = max)) in
    test_equal (count + 1) max ;;

let () = test_equal (-10000) 10000 ;;
let () = test_equal 10000 (-10000) ;;
let () = test_equal (-10000) 9999 ;;

(* less *)
let _ = assert(less (fromInt (9)) (fromInt (-10)) = false)
let _ = assert(less (fromInt (-9)) (fromInt 10) = true) 
let _ = assert(less (fromInt (9999)) (fromInt 10) = false)
let _ = assert(less (fromInt (-9999)) (fromInt 10) = true)
let _ = assert(less (fromInt (9)) (fromInt 10000) = true)
let _ = assert(less (fromInt (-9)) (fromInt 10000) = true)
let _ = assert(less (fromInt (9)) (fromInt 10) = true)
let _ = assert(less (fromInt (-9)) (fromInt (-10)) = false)
let _ = assert(less (fromInt (9000)) (fromInt 1000) = false)
let _ = assert(less (fromInt (-9000)) (fromInt (-1000)) = true)
let _ = assert(less (fromInt (9000)) (fromInt 9000) = false)
let _ = assert(less (fromInt (-9000)) (fromInt (-9000)) = false)

(* greater *)
let _ = assert(greater (fromInt (9)) (fromInt (-10)) = true)
let _ = assert(greater (fromInt (-9)) (fromInt 10) = false) 
let _ = assert(greater (fromInt (9999)) (fromInt 10) = true)
let _ = assert(greater (fromInt (-9999)) (fromInt 10) = false)
let _ = assert(greater (fromInt (9)) (fromInt 10000) = false)
let _ = assert(greater (fromInt (-9)) (fromInt 10000) = false)
let _ = assert(greater (fromInt (9)) (fromInt 10) = false)
let _ = assert(greater (fromInt (-9)) (fromInt (-10)) = true)
let _ = assert(greater (fromInt (9000)) (fromInt 1000) = true)
let _ = assert(greater (fromInt (-9000)) (fromInt (-1000)) = false)
let _ = assert(greater (fromInt (9000)) (fromInt 9000) = false)
let _ = assert(greater (fromInt (-9000)) (fromInt (-9000)) = false)

(* plus *)
let _ = assert(plus (fromInt (1)) (fromInt (1)) = fromInt (2))
let _ = assert(plus (fromInt (-1)) (fromInt (-1)) = fromInt (-2))
let _ = assert(plus (fromInt (1)) (fromInt (-1)) = fromInt (0))
let _ = assert(plus (fromInt (-1000)) (fromInt (1000)) = fromInt (0))
let _ = assert(plus (fromInt (-10)) (fromInt (1)) = fromInt (-9))
let _ = assert(plus (fromInt (1000)) (fromInt (-100)) = fromInt (900))
let _ = assert(plus (fromInt (-10)) (fromInt (100)) = fromInt (90))
let _ = assert(plus (fromInt (1000)) (fromInt (-10000)) = fromInt (-9000))

(* times *)
let _ = assert(times (fromInt (0)) (fromInt (1)) = fromInt (0))
let _ = assert(times (fromInt (-100)) (fromInt (0)) = fromInt (0))
let _ = assert(times (fromInt (2)) (fromInt (2)) = fromInt (4))
let _ = assert(times (fromInt (-5)) (fromInt (5)) = fromInt (-25))
let _ = assert(times (fromInt (5)) (fromInt (-2)) = fromInt (-10))
let _ = assert(times (fromInt (-5)) (fromInt (-5)) = fromInt (25))
let _ = assert(times (fromInt (1234)) (fromInt (2345)) = fromInt (2893730))
let _ = assert(times (fromInt (-12)) (fromInt (23456)) = fromInt (-281472))
let _ = assert(times (fromInt (100)) (fromInt (-20000)) = fromInt (-2000000))
let _ = assert(times (fromInt (-500)) (fromInt (-500)) = fromInt (250000))
let _ = assert(times {neg = false; coeffs = [123;123;123;123;123;123;123]} 
  {neg = false; coeffs = [123;123;123;123;123;123;123]} = 
  {neg = false; coeffs = 
    [15;159;303;447;591;735;879;993;849;705;561;417;273;129]})
let _ = assert(times {neg = true; coeffs = [123;123;123;123;123;123;123]} 
  {neg = false; coeffs = [123;123;123;123;123;123;123]} = 
  {neg = true; coeffs = 
    [15;159;303;447;591;735;879;993;849;705;561;417;273;129]})

