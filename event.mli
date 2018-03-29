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

(* [get_event type d] creates an event of type [type] (ex: shop, battle,
   levelup, etc) with properties defined by state [st] and optional argument [d]
   if applicable. If no [d] is passed, a default event of that type
   is returned. *)
  val get_event : string -> data -> state -> t

(* [get_attribute attr evt] is [Some v], where [v] is the value of
   the specified attribute of [evt]. If [attr] is not a valid attribute
   in event [evt], [get_attribute atter evt] is [None]. *)
  val get_attribute : string -> t -> 'a option

(* [set_attribute attr value evt] is [t] where the attribute
   with name [attr] is assigned value [value]. If [name] is not a valid
   attribute of [evt], [evt] is returned. *)
  val set_attribute : string -> 'a -> t -> t

(* [action command args st] executes the action specified by [command,]
   with the arguments in [args] if nessesary. It applies the effects of this
   action to [st], then returns the new state. *)
  val action : string -> 'a list -> state -> state

(* [end_event st] manually ends the event currently running in state and returns
   a new state object *)
  val end_event : state -> state
end
