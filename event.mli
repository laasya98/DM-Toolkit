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
  type data = D.data

(* [t] is the type of an event. *)
  type t

  type form = Battle | Shop | EditChar | Interaction

(* [role] is the role of an npc.
   A Friendly npc will aid the characters in the event
   A Hostile npc will oppose the characters in the event
   A Neutral npc will do neither (ex: shopkeeper). *)
  type role = Hostile | Friendly | Neutral

(* [make_event form d] creates an event of type [form] with properties defined
   by state [st] and optional argument [d], if applicable. If no [d] is passed,
   a default event of that type is returned. *)
  val make_event : form -> data option -> t

(* [get_type evt] is the form of the event. *)
  val get_form : t -> form

(* [get_name evt] is a string describing the name of the event. *)
  val get_name : t -> string

(* [save_event evt] creates a datatype object with the properties of
   the event *)
  val save_event : t -> data

(* [add_npc name d role evt st] adds an npc with name [name] parsed from data [d]
   with a [role] that determines its actions within the event. *)
  val add_npc : string -> data -> role -> t -> state -> (t * state)

(* [remove_npc name evt st] removes the npc with name [name] from [st] and
   [evt], and returns the new (event, state)*)
  val remove_npc : string -> t -> state -> (t * state)

(* [add_item name d evt st] adds an item to the current event in state [st]
   defined by [d] with name [name]*)
  val add_item : string -> data -> t -> state -> (t * state)

(* [remove_item name evt st] removes the item with name [name] from the
   event in state [st]*)
  val remove_item : string -> t -> state -> (t * state)

(* [action command evt st] executes the action specified by [command,].
   It applies the effects of this action to [st], then returns the new state. *)
  val action : string -> t -> state -> state
end
