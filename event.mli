open Character
open Global

(* An [Event] contains information and functions used to handle an
   event in the game, such as a shop, quest, or battle. It allows the
   state to interact with the options in the event. *)
module type Event = sig
  module C : Character

(* [character] is the type of a character in character.mli *)
  type character = Character.c

(* [t] is the type of an event. *)
  type t

(* [form] is the variety of an event. *)
  type form = Battle | Shop | Interaction

  val make_event : string -> form -> (item * quantity) list -> string list-> t

(* [get_form evt] is the form of the event. *)
  val get_form : t -> form

(* [get_name evt] is a string describing the name of the event. *)
  val get_name : t -> string

  val get_output : t -> string

(* [add_item i evt] adds an item defined by [i] to the
   event [evt]. If [i] is invalid, return [evt]. *)
  val add_item : item -> quantity -> t -> t

(* [remove_item name evt] removes the item with name [name] from the
   event in state [st]. If there is no item of that name, return [evt]. *)
  val remove_item : string -> quantity -> t -> t

(* [get_items evt] is the list of items in the event - probably only relevant
   in shops. *)
  val get_items : t -> (item * quantity) list

(* [change_form form t] changes the form of event [t] to that specified by
   [form]. This may affect fields of [t] other than just [form]. *)
  val change_form : form -> t -> t

  val attack: character -> character -> t -> (t * character)
end

module Event : Event
