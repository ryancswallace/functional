open WorldObject
open WorldObjectI
open Movable

(* ### Part 2 Movement ### *)
let cow_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_consumed_objects = 100

(** Cows will graze across the field until it has consumed a satisfactory number
    of flowers *)
class cow p hive home : movable_t =
object (self)
  inherit movable p cow_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### Part 3 Actions ### *)
  val mutable consumed_objects = 0


  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer 
    self#register_handler World.action_event (fun () -> self#do_action)


  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### Part 3 Actions ### *)
  (* method private do_action =
    if consumed_objects < max_consumed_objects then
      let neighbors = World.get self#get_pos in
      List.iter begin fun o -> match o#smells_like_pollen with
        | Some _ -> print_string "*nom* " ; flush_all () ;
          o#die ;
          consumed_objects <- consumed_objects + 1
        | None -> ()
      end neighbors *)

  (* ### TODO: Part 6 Custom Events ### *)
  method private do_action =
  let neighbors = World.get self#get_pos in
    if consumed_objects < max_consumed_objects then
      (List.iter begin fun o -> match o#smells_like_pollen with
        | Some _ -> print_string "*nom* " ; flush_all () ;
          o#die ;
          consumed_objects <- consumed_objects + 1
        | None -> ()
      end neighbors)
    else if List.mem (home :> world_object) neighbors then self#die
    

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "cow"

  method draw = super#draw_circle (Graphics.rgb 180 140 255) Graphics.black ""

  method draw_z_axis = 4


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  (* method next_direction = Helpers.with_inv_probability_or World.rand 
  (World.size / 2) (fun () -> World.direction_from_to self#get_pos hive#get_pos)
  (fun () -> Some (Direction.random World.rand)) *)

  (* ### TODO: Part 6 Custom Events ### *)
  method next_direction = 
  if consumed_objects >= max_consumed_objects then
    World.direction_from_to self#get_pos home#get_pos
  else Helpers.with_inv_probability_or World.rand 
  (World.size / 2) (fun () -> World.direction_from_to self#get_pos hive#get_pos)
  (fun () -> Some (Direction.random World.rand))

end
