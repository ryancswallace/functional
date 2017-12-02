open WorldObject
open WorldObjectI
open Bee

(** Bouncy bees will travel in a straight line in a random direction until an
    obstacle or edge of the world is reached, at which point a new random
    direction will be chosen. *)
class bee_bouncy p hive : bee_t =
object (self)
  inherit bee p hive

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Bees *)
  val mutable prev_dir = None

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Bees *)
  method get_name = "bee_bouncy"

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees *)
  method private next_direction_default =  
    let rec new_dir () = 
      let d = Direction.random World.rand in
        if World.can_move (Direction.move_point self#get_pos (Some d)) then
          (prev_dir <- Some d; Some d) else new_dir ()
    in
    match prev_dir with
    | Some dir -> if World.can_move (Direction.move_point self#get_pos 
        (Some dir)) then Some dir else new_dir ()
    | None -> new_dir ()
    
end



