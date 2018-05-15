open Database
open Event
open Character
open Command
open Global

(** A [State] is a representation of the state of the toolkit, including every
    current location and entity, and any statuses or active effects for the
    current gamespace.*)

(* [data] is the type of data in database.mli *)
  type data = Database.data

(* [character] is the type of a character in character.mli *)
  type character = Character.c

(* [event] is the type of an event in event.mli *)
  type event = Event.t

  type command = Command.command

(* [entity] is a general type for items, events, and effects
   that may be on other entities. Entities are in rooms and constitute the what
   is able to interact in the gamespace.*)
  type entity =
    |Item of item
    |Event of event

(* TODO write a spec or something*)
(* a [location] is a named structure containing a description, a character,
   a list of characters, contents, and a list of exits which link to other
   locations.*)
type location = {
  name : string;
  description : string;
  characters : (character * role) list;
  items : item list;
  event : event;
  exits : ( string * location ) list (*(direction, location)*)
}

(* [state] is the game's current state. It contains a list of locations to
   travel between, a list of currently-present characters, an active event status,
   a current location, and an output to be used in printing by Main. *)
  type state = {
    locations : location list;
    characters : (character * role) list;
    event : event;
    output :string;
    current_location : location;
  }
(* [empty_location] is an location with default record values.  *)
val empty_location : location

(* [empty_state] is a state with default record values.  *)
  val empty_state : state

(* [init_state s] is the initial state of the game loaded from a save file s
   from a database.*)
val init_state : state

(*------------------GETTERS-------------------*)

(* [current_location s] returns state with an output of current location of the toolkit's focus. *)
  val current_location : state -> state

(* [current_room_characters s] is a list of characters in the current room. *)
  val current_room_characters : state -> string list

(* [rooms s] is a list of rooms in the current gamestate. *)
  val rooms : state -> string list

(* [event s] is a list of current events for the current gamespace. *)
  val event : state -> event

(** [give st item p1 p2 q] takes in a state, an item, a character to take an
    item from, a character to give the item to, and a quantity of items to give.
    Outputs an error message if the p1 does not have the item in the correct
    quantity in their inventory, if p2 cannot hold the additional weight, or
    there is less than one item being given.*)
  val give : state -> ?q:int -> item -> character -> character -> state

(* [action cmd state] takes in a command and a state and updates the state to
   reflect whatever command is input. This can involve calling events,
   characters, items, or database look-ups.*)
  val action : command -> state -> state

(** [move st dir] moves the focus of the toolkit to the direction dir of the
        current room.*)
  val move_dir : state -> string -> state

(* [output st] is the string output of state [st], for use in testing and
   the REPL. *)
val output : state -> string

val gen_printout : state -> string
