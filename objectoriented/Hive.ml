open WEvent
open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let starting_pollen = 500
let cost_of_bee = 10
let spawn_probability = 20
let pollen_probability = 50
let max_pollen_deposit = 3

class type hive_i =
object
  inherit world_object_i

  method forfeit_honey : int -> WorldObjectI.world_object_i -> int

  method get_pollen_event : int WEvent.event
  
  method get_pollen : int
end

(** A hive will spawn bees and serve as a deposit point for the pollen that bees
    harvest.  It is possible to steal honey from a hive, however the hive will
    signal that it is in danger and its loyal bees will become angry. *)
class hive p : hive_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### Part 3 Actions ### *)
  val mutable pollen = starting_pollen


  (* ### TODO: Part 6 Custom Events ### *)
  val mutable pollen_event = WEvent.new_event ()

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event (fun () -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### Part 3 Actions ### *)
  (* ### TODO: Part 4 Aging ### *)
  method private do_action =
    Helpers.with_inv_probability World.rand pollen_probability
      begin fun () ->
        pollen <- pollen + 1
      end;
    if pollen >= cost_of_bee then
      Helpers.with_inv_probability World.rand spawn_probability
        begin fun () ->
          pollen <- pollen - cost_of_bee ;
        
  (* ### TODO: Part 4 Aging ### *)
          self#generate_bee ;
        end

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)
  
  (* method private generate_bee =
    ignore (new Bee.bee self#get_pos) *)

  (* ### TODO: Part 5 Smart Bees ### *)
  method private generate_bee = 
    Helpers.with_equal_probability (World.rand) 
      [(fun () -> World.spawn 1 self#get_pos (fun p -> 
          ignore (new BeeBouncy.bee_bouncy p (self :> world_object_i))));
       (fun () -> World.spawn 1 self#get_pos (fun p -> 
          ignore (new BeeRandom.bee_random p (self :> world_object_i))))]

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "hive"

  method draw = super#draw_circle Graphics.cyan 
    Graphics.black (string_of_int pollen)


  (* ### Part 3 Actions ### *)
  (* ### Part 6 Custom Events *)
  method receive_pollen ps =
    pollen <- pollen + (min (List.length ps) max_pollen_deposit) ;

  (* ### TODO: Part 6 Custom Events ### *)
    ignore (WEvent.fire_event self#get_pollen_event pollen) ;
    []

  (************************)
  (***** Hive Methods *****)
  (************************)

  (* ### Part 3 Actions ### *)
  method forfeit_honey n b =
    let stolen = min pollen n in
    pollen <- pollen - stolen ;
    self#danger b ;
    stolen

  (* ### TODO: Part 6 Custom Events ### *)
  method get_pollen_event = pollen_event
  
  method get_pollen = pollen

end
