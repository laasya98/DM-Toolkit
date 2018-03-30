open Database
open Event
open Character
open Command
open Item

(** A [State] is a representation of the state of the toolkit, including every
    current location and entity, and any statuses or active effects for the
    current gamespace.*)
module type State = sig
  module D : Database
  module C : Character
  module I : Item
  module E : Event

(* [data] is the type of data in database.mli *)
  type data = D.data

(* [character] is the type of a character in character.mli *)
  type character = C.c

(* [item] is the type of an item in item.mli *)
  type item = I.i

(* [event] is the type of an event in event.mli *)
  type event = E.t

(* [state] is the type of a state*)
  type state

(* [gamestate] is the type of gamestate (set of rooms and entitites to interact
   with). *)
  type gamestate

(* [entity] is a general type for items, characters, locations, and effects
   that may be on other entities. Entities are in rooms and constitute the what
   is able to interact in the gamespace.*)
  type entity =
    |Item of item
    |Character of character
    |Location of string
    |Effect of (entity * int)
    |Event of event

(* [init_state s] is the initial state of the game loaded from a save file s
   from a database.*)
  val init_state : D.data -> state

(* [current_room s] returns the current room that the DM Toolkit is focused on. *)
  val current_room : state -> string

(* [current_gamestate s] returns a description of the current gamestate. *)
  val current_gamestate : state -> string

(* [current_room_characters s] is a list of characters in the current room. *)
  val current_room_characters : state -> string list

(* [rooms s] is a list of rooms in the current gamestate. *)
  val rooms : state -> string list

(* [effects s] is a list of effects in the current room for current entities.*)
  val effects : state -> string list

(* [event s] is a list of current events for the current gamespace. *)
  val event : state -> event list


(* [action cmd state] takes in a command and a state and updates the state to
   reflect whatever command is input. This can involve calling events,
   characters, items, or database look-ups.*)
  val action : Command.command -> state -> state





end
