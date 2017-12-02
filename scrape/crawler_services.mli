(**********************************************************************
 * CS51 Problem Set 5, 2016 - Moogle
 * util.ml: an interface and the implementation of crawler services
 *          needed to build the web index, including definitions of link
 *          and page datatypes, a function for fetching a page given a 
 *          link, and the values of the command line arguments (i.e. the
 *          initial link, the number of pages to search, and the server
 *          port.)
 **********************************************************************)


open Order ;;
  
(*************************************************************)
(* signature for the services we provide for the crawler     *)
(*************************************************************)

(* links are used to describe a web address *)
type link = { host : string ;  (* e.g., "www.eecs.harvard.edu" *)
              port : int ;     (* e.g., 80 *)
              path : string    (* e.g., "/~greg/index.html" *)
            }
        
val string_of_link : link -> string
val href_of_link: link -> string
val link_compare : link -> link -> ordering
             
(* pages are used to describe the contents of web pages *)
type page = { url : link ;          (* see above *)
              links : link list ;   (* all of the links on the page *)
              words : string list   (* all of the words on the page *)
            }
val string_of_page : page -> string
(* given the link, returns the page -- should specify what
 * exceptions get raised. *)
val get_page : link -> page option
          
(* the initial link to be used by the crawler *)
val initial_link : link
(* The root directory of the server. "" if crawling the web,
    (dirname initial_link) otherwise *)
val root_dir : string
(* the number of (distinct) pages the crawler should process *)
val num_pages_to_search : int
(* the port on which to listen for query requests *)
val server_port : int
