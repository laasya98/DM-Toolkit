open Character
open Event

(* TODO: please keep this named State with the type named t.
   Also, please make sure there is at least one function allowing
   changes to be made to state by other structures.
   Delete this comment before we submit. -Kerri *)
module type State = sig
  type t
  module C : Character
  type character = C.t
  module E : Event
  type event = E.t

  val get_event : t -> E.t
end
