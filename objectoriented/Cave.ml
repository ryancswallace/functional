open WorldObject
open WorldObjectI
open Hive

(* ### Part 6 Custom Events ### *)
let spawn_bear_pollen = 500

(** A cave will spawn a bear when the hive has collected a certain amount of
    honey. *)
class cave p (hive : hive_i) : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 6 Custom Events ### *)
  val mutable honey = 0

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 6 Custom Events ### *)
  initializer
    self#register_handler hive#get_pollen_event self#do_action

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
  method private do_action pollen =
    if pollen > spawn_bear_pollen && 
    World.fold (fun o present -> o#get_name <> "bear" && present) true then
      (ignore (new Bear.bear self#get_pos hive (self :> world_object_i));
    print_string "omg bears! " ;
    flush_all () ;)
    
  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "cave"

  method draw = super#draw_circle Graphics.black Graphics.white "C"

  (* ### TODO: Part 6 Custom Events *)
  method receive_pollen pollen =
    honey <- honey + List.length pollen ;
    [] ;

end
