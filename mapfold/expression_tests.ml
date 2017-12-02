open Expression ;;
open Ast ;;
open ExpressionLibrary ;;

(* converts a float_option to a float. evaluates to sentinal value of 3.14 in
 * case of None. for use in find_0 tests *)
let float_of_float_option (opt:float option) =
    match opt with
    | Some f -> f
    | None -> 3.14 ;;

(* tests for each of the functions in (expression.ml) in comments are included
 * below. some included tests are drawn from examples given in the problem 
 * set. *)

let test () =
    (* contains_var *)
    assert (contains_var (parse "x"));
    assert (contains_var (parse "~x"));
    assert (contains_var (parse "x+3"));
    assert (contains_var (parse "cos(x)+3"));
    assert (contains_var (parse "ln(x)+3*(x-0)"));
    assert (not (contains_var (parse "4")));
    assert (not (contains_var (parse "~2")));
    assert (not (contains_var (parse "2+3-9+cos(~4)+ln(2)+(2^6)")));

    (* evaluate *)
    assert ((evaluate (parse "sin(x)") 0.0) = 0.0);
    assert ((evaluate (parse "cos(x)") 0.0) = 1.0);
    assert ((evaluate (parse "ln(x)") 10.0) > 0.99);
    assert ((evaluate (parse "ln(x)") 10.0) < 1.01);
    assert ((evaluate (parse "~x") 2.0) = (-2.0));
    assert ((evaluate (parse "5+x") (-1.0)) = 4.0);
    assert ((evaluate (parse "3-x") 0.0) = 3.0);
    assert ((evaluate (parse "5*x") 10.0) = 50.0);
    assert ((evaluate (parse "x/4") (-6.0)) = (-1.5));
    assert ((evaluate (parse "4^x") 2.0) = 16.0);
    assert ((evaluate (parse "x^4+3") 2.0) = 19.0);
    assert ((evaluate (parse "x-5+3") (-200.0)) = (-202.0));
    assert ((evaluate (parse "~cos(x)+(5*x)-(x/4)") 0.0) = (-1.0));

    (* derivative *)
    assert ((to_string (derivative (parse "9"))) = "0.");
    assert ((to_string (derivative (parse "x"))) = "1.");
    assert ((to_string (derivative (parse "sin(x)"))) = "((cos(x))*1.)");
    assert ((to_string (derivative (parse "cos(x)"))) = "((~((sin(x))))*1.)");
    assert ((to_string (derivative (parse "ln(x)"))) = "(1./x)");
    assert ((to_string (derivative (parse "~x"))) = "(~(1.))");
    assert ((to_string (derivative (parse "x+4"))) = "(1.+0.)");
    assert ((to_string (derivative (parse "x+x+x"))) = "((1.+1.)+1.)");
    assert ((to_string (derivative (parse "x-5"))) = "(1.-0.)");
    assert ((to_string (derivative (parse "x-x"))) = "(1.-1.)");
    assert ((to_string (derivative (parse "2*x"))) = "((2.*1.)+(0.*x))");
    assert ((to_string (derivative (parse "x*x"))) = "((x*1.)+(1.*x))");
    assert ((to_string (derivative (parse "2/x"))) = 
        "(((0.*x)-(2.*1.))/(x^2.))");
    assert ((to_string (derivative (parse "x/9"))) = 
        "(((1.*9.)-(x*0.))/(9.^2.))");
    assert ((to_string (derivative (parse "x^5"))) = 
        "(5.*(1.*(x^(5.-1.))))");
    assert ((to_string (derivative (parse "4^x"))) = 
        "((4.^x)*((1.*(ln(4.)))+((0.*x)/4.)))");
    assert ((to_string (derivative (parse "x^x"))) = 
        "((x^x)*((1.*(ln(x)))+((1.*x)/x)))");
    assert ((to_string (derivative (parse "sin(x^2)+4"))) = 
        "(((cos((x^2.)))*(2.*(1.*(x^(2.-1.)))))+0.)");

    (* find_zero. take not of float_of_float_option behavior wrt 3.14 *)
    assert ((float_of_float_option (find_zero (parse "x^2") 1.5 0.0001 5)) = 
        3.14);
    assert ((float_of_float_option (find_zero (parse "x^2") 1.5 0.0001 10)) = 
        0.005859375);
    assert ((float_of_float_option (find_zero (parse "(x^2)+1") 0.5 0.001 5)) = 
        3.14);
    assert ((float_of_float_option (find_zero (parse "x") 0.5 0.01 100)) = 0.);
    assert ((float_of_float_option (find_zero (parse "x+1") (-1.5) 0.01 100)) =
        (-1.));
;;

test();;
print_endline "All tests passed.";;
