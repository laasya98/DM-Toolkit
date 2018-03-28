
open State
open Database

(* An [Event] contains information and functions used to handle an
   event in the game, such as a shop, quest, or battle. It allows the
   state to interact with the options in the event. *)
module type Event = sig
(* [Parsable] is a datatype that can be parsed into values,
   such as JSON. *)
  module P : Parsable

(* [State] is the signiture of the game state. *)
  module S : State

(* [state] is the type of State in state.mli *)
  type state = S.t

(* [state] is the type of State in state.mli *)
  type parse = P.t

(* [t] is the type of an event. *)
  type t

(* [parse_event j] parses an event from parsable datatype [j]. *)
  val parse_event : parse -> t

(* [get_attribute attr evt] is [Some v], where [v] is the value of
   the specified attribute of [evt]. If [attr] is not a valid attribute
   in event [evt], [get_attribute atter evt] is [None]. *)
  val get_attribute : string -> t -> 'a option

(* [set_attribute name value evt] is [t] where the attribute
   with name [name] is assigned value [value]. If [name] is not a valid
   attribute of [evt], [evt] is returned. *)
  val set_attribute : string -> 'a -> t -> t

(* [list_options evt] lists the valid commands and their arguments
   that can be used for this event *)
  val list_options : t -> string list

(* [action command args evt st] executes the action specified by [command,]
   with the arguments in [args] if nessesary. It applies the effects of this
   action and event [evt] to state [st], then returns the new state. *)
  val event_action : string -> 'a list -> t -> state -> state
end
