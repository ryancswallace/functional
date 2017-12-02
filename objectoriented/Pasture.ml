open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let smelly_object_limit = 200

(** A pasture will spawn a cow when there are enough objects in the world that
    smell like pollen. *)
class pasture p hive : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO Part 6 Custom Events ### *)
  initializer
    self#register_handler World.action_event (fun () -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO Part 6 Custom Events ### *)
  method private do_action = 
    if (smelly_object_limit < (World.fold (fun o count ->
      if o#smells_like_pollen = None then count else count + 1) 0)) &&
    World.fold (fun o present -> o#get_name <> "cow" && present) true
      then (ignore (new Cow.cow self#get_pos hive self);
    print_string "mooooooooo " ;
    flush_all ();)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO Part 1 Basic ### *)

  method get_name = "pasture"

  method draw = super#draw_circle (Graphics.rgb 70 100 130) Graphics.white "P"



end

