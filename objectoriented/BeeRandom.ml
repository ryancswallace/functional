open WorldObject
open WorldObjectI
open Bee

(** Random bees will move randomly. *)
class bee_random p hive : bee_t =
object
  inherit bee p hive

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Bees *)
  method get_name = "bee_random"

  (***********************)
  (***** Bee METHODS *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees *)
  method private next_direction_default = Some (Direction.random World.rand)

end


