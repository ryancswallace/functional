open WorldObject
open WorldObjectI

(** Ponds serve as obstruction for other world objects. *)
class pond p : world_object_i =
object
  inherit world_object p as super

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "pond"

  method draw = super#draw_circle Graphics.blue Graphics.black "" 

  method is_obstacle = true

end
