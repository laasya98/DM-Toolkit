(* TODO: replace with actual one *)
module type State = sig
  type t
end

module type Parsable = sig
  type t

  val member : string -> t -> t

  val to_bool : t -> bool

  val to_int : t -> int

  val to_string : t -> string
end

module type Event = sig
  module St : State
  module Par : Parsable
  type evt

(* [state] is the type of a state from State.mli *)
  type state = St.t

  type parse = Par.t

  val parse_event : parse -> evt

  val event_details : evt -> string

(* [list_options evt] lists the valid commands and their arguments
   that can be used for this event *)
  val list_options : evt -> string list

(* [action command args evt st] executes the action specified by [command,]
   with the arguments in [args] if nessesary. It applies the effects of this
   action and event [evt] to state [st], then returns the new state. *)
  val event_action : string -> 'a list -> evt -> state -> state

  val event_ended : evt -> bool

end

module type EventMaker =
  functor (S : State)
    -> functor (P : Parsable)
    -> Event with module St=S and module Par = P
