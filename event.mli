open Character
open Global

(* [character] is the type of a character in character.mli *)
  type character = Character.c

(* [t] is the type of an event.
   An event contains information and functions used to handle an
   event in the game, such as a shop, quest, or battle. It allows the
   state to interact with the options in the event. *)
  type t

(* [form] is the variety of an event. *)
  type form = Battle | Shop | Interaction

(* [init_event n c] returns a basic empty event with initial fields and
   form Interaction. Turn order is based on [c]. *)
val init_event : string -> t

(* [parse_event datalst] parses an event from associative list [datalst].
   raises: "Invalid Event Data" if the list doesn't contain the requred
    information. *)
   val parse_event : (string*string) list -> t

(* [make_event n f is cs] returns an event with name [n], form [f], items [is],
   and turn order of characters [cs]. Output is set to "Event created."
   All other fields are initialized to default values. *)
val make_event : string -> form -> (item * quantity) list ->
  character list -> t

val verbose : t -> string

val clear_vout : t -> unit

(* [get_form evt] is the form of the event. *)
  val get_form : t -> form

(* [get_name evt] is a string describing the name of the event. *)
  val get_name : t -> string

(* [get_output evt] is a string describing changes made to the event
   by the last operation. *)
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

(* [attack a t evt] calculates the result of character [a] attacking
   character [t], and returns the new states of [evt] and [t]. *)
  val attack: character -> character -> t -> (t * character)

(* [turn evt] calculates the result of advancing the game by a single
   turn. This may cause delayed effects, such as spells, to trigger.
   returns: the new states of [evt] and a list of changes to characters. *)
  val turn : t -> (t * character list)

(* [get_turn evt] is the number of the current turn. *)
  val get_turn : t -> int

(* [get_turnlst evt] is the list of character names in order of their turns.
   The character whose turn it is is listed at the head of the list. *)
  val get_turnlst : t -> string list

(* [cast c s ts evt] is the result of character [c] casting spell [s] with
   targets [ts] in combat event [evt].
   If the spell requires multiple turns to cast, it is added to the spell list
   in [evt], which will call [cast] again later when it is ready through
   the function [turn].
   returns: the new state of [evt] and [ts] after the spell is either cast or
   deferred to a later turn. *)
  val cast : character -> spell -> character list -> t -> (t * character list)

(* [get_waiting_spells evt] returns an association list of the waiting spells
   and the turn on which they will be cast. *)
val get_waiting_spells : t -> (spell*int) list

val use_item: item -> character -> t -> (t * character list)
