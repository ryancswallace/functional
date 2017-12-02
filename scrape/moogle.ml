(**********************************************************************
 * CS51 Problem Set 5, 2016 - Moogle
 * moogle.ml: the main code for the Moogle server
 **********************************************************************)

open Crawler_services
open Pagerank
open Crawl

(* Set to true to enable various debugging printouts *)
let debug = false ;;
let time = false ;;

let std_response_header =
  "HTTP/1.1 200 OK\r\n"
  ^ "Server: Moogle_soln/0.0\n"
  ^ "content-type: text/html; charset=utf-8\n"
  ^ "Content-Language: en-us\n"
  ^ "Connection: close\n\n" ;;

let moogle_home_page = "./moogle.html" ;;

(* read in all the lines from a file and concatenate them into
 * a big string. *)
let rec input_lines inchan lines =
  try
    input_lines inchan ((input_line inchan)::lines)
  with End_of_file -> List.rev lines ;;

(* Read the contents of a webpage.
 * page : string, a filename*)
let read_page page =
  let _ = Printf.printf "reading '%s'\n" page in
  let _ = flush_all() in
  let ch = open_in page in
  let lines = input_lines ch [] in
  let resp = String.concat "" lines in
    close_in ch ; resp ;;

(* Build a message that has the default Moogle home page to send
 * to clients.  The contents of the home page can be found in
 * the file moogle.html. *)
let std_response =
  read_page moogle_home_page ;;

(* The header for search responses to clients. *)
let query_response_header time num =
  std_response_header ^
    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" ^
    "<html> <head> <title>Moogle search results </title></head>" ^
    "<body><h1>Search results:</h1>" ^
    "<p>" ^ string_of_int num ^ " results (" ^ (Printf.sprintf "%.5f" time) ^
    " seconds)</p><p><ul>" ;;

(* Convert a set of url's to HTML to splice into the search
 * response we send to clients. *)
let html_of_urllist (links: link list) ranks : string =
  let deoptionalize opt def =
    match opt with
    | Some x -> x
    | None -> def in
  List.fold_left 
    (fun s link -> "<li>"
       ^ (Printf.sprintf "%0.*f" 4
             (deoptionalize (RankDict.lookup ranks link) 0.0))
       ^ " <a href=\""
       ^ (href_of_link link) ^ "\">"
       ^ (string_of_link link)
       ^ "</a></li>" ^ s)
    "" links ;;

(* The footer for search responses to clients. *)
let query_response_footer = "</ul><hr></body></html>" ;;

let send_std_response client_fd =
  Unix.send client_fd std_response 0 (String.length std_response) [] ;;

let http_get_re =
  Str.regexp_case_fold "GET[ \t]+/\\([^ \t]*\\)[ \t]+HTTP/1\\.[0-9]" ;;

let sort_by_rank (links:LinkSet.set) (ranks : RankDict.dict) : link list =
  let compare_links a b =
    match (RankDict.lookup ranks a, RankDict.lookup ranks b) with
      | (Some x, Some y) -> compare x y
          (* No ranking is lower than any rank item *)
      | (Some _, None) -> 1
      | (None, Some _) -> -1
      | (None, None) -> 0
  in
  let links_list = LinkSet.fold (fun l x -> x :: l) [] links in
    List.sort compare_links links_list ;;

let do_query query_string index ranks =
  let start = Unix.gettimeofday () in
  let query = Q.parse_query query_string in
  let links = Q.eval_query index query in
  let num = LinkSet.fold (fun l _x -> 1 + l) 0 links in
  let sorted_links = sort_by_rank links ranks in
  let response_body = html_of_urllist sorted_links ranks in
  let finish = Unix.gettimeofday () in
  let time = finish -. start in
    ((query_response_header time num) ^ response_body ^ query_response_footer,
     time);;

(* Given a requested path, return the corresponding local path *)
let local_path qs =
  Filename.concat root_dir qs ;;

let send_all fd buf =
  let rec more st size =
    let res = Unix.send fd buf st size [] in
    if res < size then
      more (st + res) (size - res)
    else ()
  in
  let size = String.length buf in
  let _ = more 0 size in size ;;

(* process a request -- we're expecting a GET followed by a url or a query
 * "?q=word+word".  If we find a query, then we feed it to the query parser to
 * get query abstract syntax.  Then we evaluate the query, using the index we
 * built earlier, to get a set of links.  Then we put the result in an html
 * document to send back to the client.
 *
 * If we find a url, we try to send back the correponding file.
 *
 * If we don't understand the request, then we send the default page (which is
 * just moogle.html in this directory).
 *)
let process_request client_fd request index ranks =
  (*  let _ = Printf.printf "Request: %s\n----\n" request in
      let _ = flush_all() in *)
  let is_search qs =
    let r = Str.regexp_string "?q=" in
      Str.string_match r qs 0
  in
  let is_safe s =
    (* At least check that the passed in path doesn't contain .. *)
    let r = Str.regexp_string ".." in
      try
        let _ = Str.search_forward r s 0 in
          false
      with Not_found -> true
  in
    try
      let _ = Str.search_forward http_get_re request 0 in
      let query_string = Str.matched_group 1 request in
      (*
      let _ = Printf.printf "Query string: '%s'\n\n" query_string in
      let _ = flush_all() in *)
      let response =
        if is_search query_string then
          let (str, _) = do_query query_string index ranks
          in str
        else
          if is_safe query_string
          then read_page (local_path query_string)
    else (Printf.printf "%s" "not safe!";
    flush_all();
    std_response)
      in
      send_all client_fd response
    with _ -> send_std_response client_fd ;;

let time_query (index : WordDict.dict) (ranks : RankDict.dict) =
  let (_, time1) = do_query "?q=a" index ranks in
  let (_, time2) = do_query "?q=in" index ranks in
  let (_, time3) = do_query "?q=the" index ranks in
  let (_, time4) = do_query "?q=and" index ranks in
  let (_, time5) = do_query "?q=moo" index ranks in
  let (_, time6) = do_query "?q=set" index ranks in
  let _ = print_string "Average query time: " in
  let _ = Printf.printf "%f" ((time1 +. time2 +. time3 +. time4 +. time5 +. 
    time6) /. 6.0) in
  let _ = print_string "\n" in
  flush_all () 

let time_crawls () = 
  let crawl_single pages path = 
    let start = Unix.gettimeofday () in
    let _ = crawl pages 
    (PR.LinkSet.singleton {host = ""; port = 80; path = path})
      PR.LinkSet.empty WordDict.empty in
    let finish = Unix.gettimeofday () in
      Printf.printf "Crawling %d pages from %s took %f seconds\n" pages path
       (finish -. start);
      flush_all ();
  in
  crawl_single 7 "simple-html/index.html";
  crawl_single 7 "html/index.html";
  crawl_single 7 "wiki/Teenage_Mutant_Ninja_Turtles";
  crawl_single 20 "html/index.html";
  crawl_single 20 "wiki/Teenage_Mutant_Ninja_Turtles";
  crawl_single 224 "wiki/Teenage_Mutant_Ninja_Turtles"
  ;;

(* open a socket on the server port (specified on the command line),
 * prepare it for listening, and then loop, accepting requests and
 * sending responses.
 *)
let server (index:WordDict.dict) (ranks:RankDict.dict) =
  let _ = Printf.printf "Starting Moogle on port %d.\n" server_port in
  let _ = Printf.printf "Press Ctrl-c to terminate Moogle.\n" in
  let _ = flush_all () in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in
  let _ = Unix.setsockopt fd Unix.SO_REUSEADDR true in
  let _ = Unix.bind fd sock_addr in
  let _ = Unix.listen fd 5 in  (* at most 5 queued requests *)
  let rec server_loop () =
    (* allow a client to connect *)
    let (client_fd, _) = Unix.accept fd in
    let buf = Bytes.create 4096 in
    let len = Unix.recv client_fd buf 0 (String.length buf) [] in
    let request = String.sub buf 0 len in
    let _ = process_request client_fd request index ranks in
      Unix.close client_fd ;
      server_loop() in
      let _ = if time then
        time_query index ranks 
      else () in 
    server_loop() ;;


let print_dict fold sk sv d =
  fold (fun _ k v ->
        Printf.printf "key: %s; value: (%s)\n" (sk k) (sv v)) () d ;;

let debug_pageranks ranks =
  if debug then
    (Printf.printf "\nRanks:\n";
     print_dict RankDict.foldl
    RankDict.string_of_key
    RankDict.string_of_value
    ranks;
     flush_all())
  else () ;;

let debug_index index =
  if debug then (Printf.printf "\nIndex:\n";
                 print_dict WordDict.foldl WordDict.string_of_key
          WordDict.string_of_value index;
     flush_all())
  else () ;;

let compute_pagerank (index : WordDict.dict) : RankDict.dict =
  let links = WordDict.foldl (fun l _ v -> LinkSet.union l v)
           LinkSet.empty index in
  let pages = LinkSet.fold
    (fun s link ->
     match get_page link with
     | None -> s
     | Some page -> PageSet.insert page s) PageSet.empty links in
  let link_graph = graph_of_pages pages in
  let _ = if debug then Printf.printf "link_graph = %s"
              (PageGraph.string_of_graph link_graph) else () in
  dict_of_ns (MoogleRanker.rank link_graph) ;;

let time_crawler crawler arg =
  let start = Unix.gettimeofday () in
  let res = crawler arg in
  let finish = Unix.gettimeofday () in
    Printf.printf "Crawling took %f seconds...\n" (finish -. start);
    res ;;

let main () =
  (* Want different random numbers every time. *)
  let _ = Random.self_init () in
    (* Construct the index to pass to the server *)
  let _ = flush_all () in
  let _ = Printf.printf "Indexing %d pages.\n" num_pages_to_search in
  let index = time_crawler crawler () in
  let _ = if not (time) then
    time_crawls () in
  let _ = Printf.printf "Index has been constructed.\n" in
  let _ = debug_index index in
  let _ = Printf.printf "Computing page ranks.\n" in
  let ranks = compute_pagerank index in
  let _ = debug_pageranks ranks in
  let _ = Printf.printf "Page ranks computed.\n" in
  server index ranks ;;

(* run Moogle server only in production, not testing *)
try
  let _ = Str.search_forward (Str.regexp "moogle\\.\\(byte\\|native\\)")
   (Sys.argv.(0)) 0 in
  main ()
with Not_found -> Printf.printf "Running from test; will not start server.\n" 
;;
