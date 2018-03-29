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

  type role = Hostile | Friendly | Neutral

(* [make_event type d] creates an event of type [type] (ex: shop, battle,
   levelup, etc) with properties defined by state [st] and optional argument [d]
   if applicable. If no [d] is passed, a default event of that type
   is returned. *)
  val make_event : string -> data -> state -> t

(* [get_type evt] is a string describing the type of the event. *)
  val get_type : t -> string

(* [get_name evt] is a string describing the name of the event. *)
  val get_name : t -> string

(* [save_event evt] creates a datatype object with the properties of
   the event *)
  val save_event : t -> data

(* [add_npc name d role st] adds an npc with name [name] parsed from data [d]
   with a [role] that determines its actions within the event. *)
  val add_npc : string -> data -> role -> state -> state

(* [remove_npc name st] removes the npc with name [name] from [st].*)
  val remove_npc : string -> state -> state

(* [action command args st] executes the action specified by [command,]
   with the arguments in [args] if nessesary. It applies the effects of this
   action to [st], then returns the new state. *)
  val action : string -> 'a list -> state -> state

(* [end_event st] manually ends the event currently running in state and returns
   a new state object *)
  val end_event : state -> state
end
