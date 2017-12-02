
(** Abstract syntax of MiniML expressions *)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(** Sets of varids *)
module SS = Set.Make(struct
          type t = varid
          let compare = String.compare
        end);;
  
type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;
  
(** Return a set of the variable names free in [exp] *)
let free_vars (exp : expr) : varidset =
  let vlist = SS.empty in 
    let rec add_vars (exp : expr) (vset : varidset) = 
      match exp with 
      | Var v -> SS.add v vset
      | Unop (v, e) -> add_vars e vset
      | Binop (v, e1, e2) -> add_vars e2 (add_vars e1 vset)
      | Fun (v, e) -> SS.remove v (add_vars e vset)
      | Let (v, e1, e2) -> add_vars e1 (SS.remove v (add_vars e2 vset))
      | Letrec (v, e1, e2) -> SS.remove v (add_vars e2 (add_vars e1 vset))              
      | Conditional (e1, e2, e3) -> add_vars e3 (add_vars e2 (add_vars e1 vset))
      | App (e1, e2) -> add_vars e2 (add_vars e1 vset) 
      | Num _ | Bool _ -> vset
      | Raise -> failwith "Exception raised"
      | Unassigned -> failwith "Unassigned"
  in add_vars exp vlist
;;
  
(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname =
  let count = ref 0 in
    fun () ->
      count := !count + 1;
      "var" ^ string_of_int (!count - 1)
;;
  
(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  match exp with
  | Var v -> if v = var_name then repl else Var v             
  | Num i -> Num i                     
  | Bool b -> Bool b                  
  | Unop (v, e) -> Unop (v, (subst var_name repl e))          
  | Binop (v, e1, e2) -> Binop (v, (subst var_name repl e1), 
      (subst var_name repl e2))   
  | Conditional (e1, e2, e3) -> Conditional ((subst var_name repl e1), 
      (subst var_name repl e2), (subst var_name repl e3)) 
  | Fun (v, e) -> if v = var_name then Fun (v, e)
    else if not (SS.mem v (free_vars repl)) then Fun (v, 
      (subst var_name repl e))
    else let nvar = new_varname () in
      let e' = subst v (Var nvar) e in
        Fun (nvar, (subst var_name repl e'))         
  | Let (v, e1, e2) -> if v = var_name then Let (v, (subst var_name repl e1), 
      e2)
    else if not (SS.mem v (free_vars repl)) then Let (v, 
      (subst var_name repl e1), (subst var_name repl e2))
    else let nvar = new_varname () in
      let e1' = subst v (Var nvar) e1 in
        Let (nvar, (subst var_name repl e1'), (subst var_name repl e2))
  | Letrec (v, e1, e2) -> if v = var_name then Letrec (v, e1, e2)
    else Letrec (v, (subst var_name repl e1), (subst var_name repl e2))   
  | Raise -> Raise                           
  | Unassigned -> Unassigned                      
  | App (e1, e2) -> App ((subst var_name repl e1), (subst var_name repl e2))

 (* Returns a string representation of the expr either pretty printed for 
  * output if pretty is true or as the abstract synatx tree if pretty is
  * false *)
let exp_to_string_f (exp : expr) (pretty : bool) : string =
  let rec to_string (exp : expr) : string =
    match exp with 
    | Var v -> if pretty then v else "Var(" ^ v ^ ")"                     
    | Num i -> if pretty then string_of_int i 
        else "Num(" ^ string_of_int i ^ ")"                   
    | Bool b -> if pretty then string_of_bool b 
        else "Bool(" ^ string_of_bool b ^ ")"           
    | Unop (v, e) -> if pretty then v ^ (to_string e) 
        else "Unop(" ^ v ^ ", " ^ (to_string e) ^ ")"   
    | Binop (v, e1, e2) -> if pretty then 
          (to_string e1) ^ " " ^ v ^ " " ^ (to_string e2) 
        else "Binop(" ^ v ^ ", " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")" 
    | Conditional (e1, e2, e3) -> if pretty then "if " ^ (to_string e1) ^ 
          " then " ^ (to_string e2) ^ " else " ^ (to_string e3) 
        else "Conditional(" ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ", " ^ 
          (to_string e3) ^ ")"   
    | Fun (v, e) -> if pretty then "fun " ^ v ^ " -> " ^ (to_string e) 
        else "Fun(" ^ v ^ ", " ^ (to_string e) ^ ")"               
    | Let (v, e1, e2) ->  if pretty then "let " ^ v ^ " = " ^ (to_string e1) 
          ^ " in " ^ (to_string e2) 
        else "Let(" ^ v ^ ", " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"      
    | Letrec (v, e1, e2) -> if pretty then "let rec " ^ v ^ " = " ^ 
          (to_string e1) ^ " in " ^ (to_string e2) 
        else "Letrec(" ^ v ^ ", " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")" 
    | Raise -> failwith "Exception raised"                        
    | Unassigned -> "Unassigned"                       
    | App (e1, e2) -> if pretty then (to_string e1) ^ " (" ^ (to_string e2) ^ 
        ")" else "App(" ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")" 
  in to_string exp
;;

(** Returns a string representation of the expr *)
let rec exp_to_string (exp : expr) : string =
  exp_to_string_f exp false   
;;
