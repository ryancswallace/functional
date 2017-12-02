open WorldObject
open WorldObjectI
open Movable
open Ageable
open CarbonBased

(* ### Part 2 Movement ### *)
let bee_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_pollen_types = 5

(* ### Part 4 Aging ### *)
let bee_lifetime = 1000

(* ### Part 5 Smart Bees ### *)
let max_sensing_range = 5

class type bee_t =
object 
  inherit Ageable.ageable_t

  method private next_direction_default : Direction.direction option
end 

(** Bees travel the world searching for honey.  They are able to sense flowers
    within close range, and they will return to the hive once they have
    pollenated enough species of flowers. *)
class bee p (home : world_object_i) : bee_t =
object (self)
  inherit carbon_based p bee_inverse_speed (World.rand bee_lifetime) 
    bee_lifetime as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### Part 3 Actions ### *)
  val mutable pollen = []

  (* ### TODO: Part 5 Smart Bees ### *)
  val sensing_range = World.rand max_sensing_range
  
  val pollen_types = World.rand max_pollen_types + 1

  (* ### TODO: Part 6 Custom Events ### *)
  val mutable offender = None

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer 
    self#register_handler World.action_event (fun () -> self#do_action) ;

  (* ### TODO: Part 6 Custom Events ### *)
    self#register_handler home#get_danger_event self#fight_danger

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  method private do_action : unit =
    let placid () = 
      let neighbors = World.get self#get_pos in
        List.iter self#deposit_pollen neighbors ;
        List.iter self#extract_pollen neighbors in
    match offender with 
    | None -> placid ()
    | Some thief -> (if thief#get_pos = self#get_pos then 
        (thief#receive_sting ; 
        self#die ));
        placid () ;

  (* ### TODO: Part 6 Custom Events ### *)
  method private fight_danger thief : unit =
    offender <- Some thief; 
    self#register_handler thief#get_die_event self#call_off

  (* ### TODO: Part 6 Custom Events ### *)
  method private call_off () : unit = 
    offender <- None

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### Part 3 Actions ### *)
  method private deposit_pollen (o:world_object_i) : unit =
    let pollen' = o#receive_pollen pollen in
    pollen <- pollen'

  method private extract_pollen (o:world_object_i) : unit =
    match o#forfeit_pollen with
    | None -> ()
    | Some i -> pollen <- i::pollen

  (* ### TODO: Part 5 Smart Bees ### *)
  method private magnet_flower : world_object_i option =
    let check_fl fl = match fl#smells_like_pollen with
      | None -> false
      | Some id -> not (List.mem id pollen) in
    let nearby = List.filter check_fl (World.objects_within_range  
      self#get_pos sensing_range) in 
        if List.length nearby = 0 then None 
        else let less fl1 fl2 = 
          if Direction.distance self#get_pos fl1#get_pos <= Direction.distance 
          self#get_pos fl2#get_pos then fl1 else fl2
        in Some (List.fold_left less (List.hd nearby) nearby)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "bee"

  (* method draw = super#draw_circle Graphics.yellow Graphics.black (
    string_of_int (List.length pollen)) *)

  method draw_z_axis = 2


  (* ### TODO: Part 4 Aging ### *)
  method draw_picture = super#draw_circle Graphics.yellow Graphics.black 
    (string_of_int (List.length pollen))

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  (* method next_direction = Some (Direction.random World.rand) *)

  (* ### TODO: Part 5 Smart Bees ### *)

  (* method next_direction = 
    let unique l p = if List.mem p l then l else p::l in
      if List.length (List.fold_left unique [] pollen) > pollen_types
      then World.direction_from_to self#get_pos home#get_pos
      else match self#magnet_flower with
      | Some fl -> World.direction_from_to self#get_pos fl#get_pos
      | None -> self#next_direction_default *)

  (* ### TODO: Part 6 Custom Events ### *)
  method next_direction =
    match offender with 
    | None -> 
      (let unique l p = if List.mem p l then l else p::l in
        if List.length (List.fold_left unique [] pollen) > pollen_types then
          World.direction_from_to self#get_pos home#get_pos
        else match self#magnet_flower with
        | Some fl -> World.direction_from_to self#get_pos fl#get_pos
        | None -> self#next_direction_default)
    | Some thief -> World.direction_from_to self#get_pos thief#get_pos


  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees ### *)
  method private next_direction_default = Some (Direction.random World.rand)

end
