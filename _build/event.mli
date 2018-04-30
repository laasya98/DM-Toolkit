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

(* [role] is the role of an npc.
   A Friendly npc will aid the characters in the event
   A Hostile npc will oppose the characters in the event
   A Neutral npc will do neither (ex: shopkeeper). *)
  type role = Hostile | Friendly | Neutral

(* [npc] is the type of an npc. It wraps the Character type with a role and
   a tag. *)
  type npc = {details:character; role:role; tag:int}

  val make_event : string -> form -> npc list -> (item * quantity) list -> t

(* [get_form evt] is the form of the event. *)
  val get_form : t -> form

(* [get_name evt] is a string describing the name of the event. *)
  val get_name : t -> string

  val get_output : t -> string

(* [add_npc name c role evt] adds an npc with name [name],
   character [c], and [role] that determines its actions within the event.
   If [d] is invalid, return [evt]. *)
  val add_npc : character -> role -> t -> t

(* [remove_npc tag evt] removes the npc with tag [tag] from [evt], and returns
   the new [evt]. If there is no npc of that tag, return [evt]. *)
  val remove_npc : int -> t -> t

  val update_npc : npc -> ?c:character -> ?r:role -> ?o:string -> t -> t

(* [get_npcs evt] is a list of the NPCs present in the event. *)
  val get_npcs : t -> npc list

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

  val attack_opt : character option -> string -> character option -> string -> t
    -> (t * character option)
end

module Event : Event
