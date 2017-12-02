open Mapfold ;;
 
(* tests for each of the functions in (mapfold.ml) in comments are included
 * below. some included tests are drawn from examples given in the problem 
 * set. *)

let test () =
    (* negate_all *)
    assert ((negate_all []) = []);
    assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);

    (* sum *)
    assert ((sum []) = 0);
    assert ((sum [0]) = 0);
    assert ((sum [0;-1;1]) = 0);
    assert ((sum [0;400;-300;200]) = 300);

    (* sum_rows *)
    assert ((sum_rows [[]; []]) = [0;0]);
    assert ((sum_rows [[-1]; [3]]) = [-1;3]);
    assert ((sum_rows [[1;2]; [3;4]]) = [3;7]);
    assert ((sum_rows [[1;2;-100]; [3;400]]) = [-97;403]);
    assert ((sum_rows [[1;200;-100]; []]) = [101;0]);

    (* filter_odd *)
    assert ((filter_odd []) = []);
    assert ((filter_odd [1]) = [1]);
    assert ((filter_odd [2]) = []);
    assert ((filter_odd [-101]) = [-101]);
    assert ((filter_odd [-10]) = []);
    assert ((filter_odd [10;4;5;-3]) = [5;-3]);

    (* num_occurs *)
    assert ((num_occurs 1 []) = 0);
    assert ((num_occurs 1 [1]) = 1);
    assert ((num_occurs 100 [-100]) = 0);
    assert ((num_occurs (-800) [-800;800;-800;800;-800;800;-800;800]) = 4);
    assert ((num_occurs 4 [-1;3;-4;5;4]) = 1);

    (* super_sum *)
    assert ((super_sum [[]]) = 0);
    assert ((super_sum [[-100]]) = -100);
    assert ((super_sum [[-100];[100];[0]]) = 0);
    assert ((super_sum [[-100;101];[100;-101;50];[0]]) = 50);
    assert ((super_sum [[1;2;3];[];[5]]) = 11);

    (* filter_range *)
    assert ((filter_range [] (-1,-1)) = []);
    assert ((filter_range [] (-100,100)) = []);
    assert ((filter_range [5;-5] (-1,-1)) = []);
    assert ((filter_range [1;3;4;5;2] (1,-1)) = []);
    assert ((filter_range [5;5;5] (5,5)) = [5;5;5]);
    assert ((filter_range [500;500;500] (-500,500)) = [500;500;500]);
    assert ((filter_range [1;3;4;5;2] (1,1)) = [1]);
    assert ((filter_range [-1;3;4;500;2] (1,300)) = [3;4;2]);
    assert ((filter_range [-1;300;4000;-500;0] (-500,300)) = [-1;300;-500;0]);

    (* floats_of_ints *)
    assert ((floats_of_ints []) = []);
    assert ((floats_of_ints [1]) = [1.]);
    assert ((floats_of_ints [-1]) = [-1.]);
    assert ((floats_of_ints [-1;100;0]) = [-1.0;100.;0.0]);

    (* log10s *)
    assert ((log10s [0.0]) = [None]);
    assert ((log10s [10.0]) = [Some 1.0]);
    assert ((log10s [-1.0]) = [None]);
    assert ((log10s [1000.0]) = [Some 3.0]);
    assert ((log10s [1.0; 10.0; -10.0]) = [Some 0.; Some 1.; None]);

    (* deoptionalize *)
    assert ((deoptionalize []) = []);
    assert ((deoptionalize [Some 100000]) = [100000]);
    assert ((deoptionalize [Some (-100000)]) = [-100000]);
    assert ((deoptionalize [None]) = []);
    assert ((deoptionalize [Some 3; None; Some (-5); Some 10]) = [3;-5;10]);

    (* some_sum *)
    assert ((some_sum []) = 0);
    assert ((some_sum [None]) = 0);
    assert ((some_sum [Some 1000]) = 1000);
    assert ((some_sum [Some (-1000)]) = -1000);
    assert ((some_sum [Some (-500); Some 10; None; Some 2; None]) = -488);

    (* mult_odds *)
    (* note that it is reasonable for the empty list to evaluate to 1,
     * as 1 is the multiplicative identity *)
    assert ((mult_odds []) = 1);
    assert ((mult_odds [5]) = 5);
    assert ((mult_odds [-5]) = -5);
    assert ((mult_odds [0]) = 1);
    assert ((mult_odds [2]) = 1);
    assert ((mult_odds [1;3;0;2;-5]) = -15);

    (* concat *)
    assert ((concat [[]]) = []);
    assert ((concat [[];[];[]]) = []);
    assert ((concat [[2];[1];[-2]]) = [2;1;-2]);
    assert ((concat [[2;90];[1;-9];[-2;8]]) = [2;90;1;-9;-2;8]);
    assert ((concat [[2];[1;-9];[-2]]) = [2;1;-9;-2]);
    assert ((concat [[2.0;90.5];[1.0;-9.5];[-2.1;8.0]]) = 
        [2.0;90.5;1.0;-9.5;-2.1;8.0]);
    assert ((concat [["a";"asdf"];["a"]]) = ["a";"asdf";"a"]);

    (* filter_by_year *)
    assert ((filter_by_year [("Joe",2010);("Bob",2010);("Tom",2013)] 2010) = 
        ["Joe";"Bob"]);
    assert ((filter_by_year [("Joe",2010);("Bob",2010);("Tom",0)] 0) = 
        ["Tom"]);
    assert ((filter_by_year [("tom",-500);("dick",2200);("larry",2400)] 2200) =
        ["dick"]);
    assert ((filter_by_year [("tom",-500);("dick",2200);("larry",2400)] 2300) =
        []);
    assert ((filter_by_year [("tom22",-500);("dick",2200);("larry",-500)] 
        (-500)) = ["tom22";"larry"]);
    
;;

test();;
print_endline "All tests passed.";;
