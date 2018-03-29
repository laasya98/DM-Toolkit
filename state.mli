open Character

(* TODO: please keep this named State with the type named t.
   Also, please make sure there is at least one function allowing
   changes to be made to state by other structures.
   Delete this comment before we submit. -Kerri *)
module type State = sig
  type t
  module C : Character
  type character = C.t

(* [get_attribute attr st] is [Some v], where [v] is the value of
   the specified attribute of [evt]. If [attr] is not a valid attribute
   in event [evt], [get_attribute atter evt] is [None].
   TODO: you can do this differently if you want, as long as there is
something similar. - Kerri *)
  val get_attribute : string -> t -> 'a option

(*    TODO: you can do this differently if you want, as long as there is
  something similar. - Kerri *)
  val set_attribute : string -> 'a option -> t -> t
end
