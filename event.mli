open State
open Database
open Character

(* An [Event] contains information and functions used to handle an
   event in the game, such as a shop, quest, or battle. It allows the
   state to interact with the options in the event. *)
module type Event = sig
(* [D] is the signiture of the game database. *)
  module D : Database

(* [State] is the signiture of the game state. *)
  module S : State

(* [state] is the type of State in state.mli *)
  type state = S.t

(* [data] is the type of data in database.mli *)
  type data = D.t

(* [t] is the type of an event. *)
  type t

  (* [start_event d] creates an event with properties defined by [d]. *)
  val start_event : data -> t

(* [get_attribute attr evt] is [Some v], where [v] is the value of
   the specified attribute of [evt]. If [attr] is not a valid attribute
   in event [evt], [get_attribute atter evt] is [None]. *)
  val get_attribute : string -> t -> 'a option

(* [get_output evt] is the output that should be displayed at this point
   in the event. For use by State. *)
  val get_output : t -> string

(* [set_attribute name value evt] is [t] where the attribute
   with name [name] is assigned value [value]. If [name] is not a valid
   attribute of [evt], [evt] is returned. *)
  val set_attribute : string -> 'a -> t -> t

(* [list_options evt] lists the valid commands and their arguments
   that can be used for this event *)
  val list_options : t -> string list

(* [action command args evt st] executes the action specified by [command,]
   with the arguments in [args] if nessesary. It applies the effects of this
   action to [evt], then returns the new event state. *)
  val event_action : string -> 'a list -> t -> t

  val load_changes : state -> t -> state
end

module type Combat = sig
  type t
  include Event with type t := t
  module C : Character
  type character = C.t

  val start_combat : data -> character list -> t

(* [add_enemy name evt] *)
  val add_enemy : string -> t -> t

  val remove_enemy : string -> t -> t

(* [attack caster action targets evt] *)
  val attack : string -> string -> string list -> t -> t

  val stall : string -> t -> t
end
