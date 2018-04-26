open Database
open Event
open Character
open Command
open Global

(** A [State] is a representation of the state of the toolkit, including every
    current location and entity, and any statuses or active effects for the
    current gamespace.*)
module type State = sig
  module D : Database
  module C : Character
  module E : Event
  module Com :Command

  type data = D.data
  type character = C.c
  type event = E.t
  type location =

  type state

  type entity =
    |Item of item
    |Character of character
    |Effect of (entity * int)
    |Event of event

  type exits = {}

  type location = {
    name : string;
    description : string;
    contents : entity list;
    exits : ( string * location ) list (*(direction, location)*)
    }

  type gamestate = {
    locations : location list;
    party : character list;
    active_events : event list;
  }

  val init_state : D.data -> state
  val current_room : state -> string
  val current_gamestate : state -> string
  val current_room_characters : state -> string list
  val rooms : state -> string list
  val effects : state -> string list
  val event : state -> event list
  val action : Com.command -> state -> state
end

module MakeState
    = functor (C:Character) -> functor (D:Database) -> functor (E:Event) ->
      functor (Com:Command) ->
    struct

    type data = D.data
    type character = C.c
    type event = E.t

    type state = {
      event:event;
      characters: character list;
    }

(*************************** KERRI STUFF BELOW *****************************)

  (** SHOP **)
let buy_item name evt =
  match List.find_opt (fun x -> x.name =name) (E.get_items evt) with
  | Some i -> failwith "unimplemented"
  | None -> failwith "unimplemented"

(** COMBAT **)

let attack a t evt st =
  let at = List.find_opt (fun x -> C.name x = a) st.characters in
  let t = List.find_opt (fun x -> C.name x = t) st.characters in
  match t with
  | None -> failwith "update event only"
  | Some c -> failwith "update character effected"

(*************************** KERRI STUFF ABOVE *****************************)

end
