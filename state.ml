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

  type state

  type entity =
    |Item of item
    |Character of character
    |Effect of (entity * int)
    |Event of event

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

module State = struct
  module D = Database
  module C = Character
  module E = Event
  module Com = Command

  type character = C.c
  type event = E.t

  type entity =
    |Item of item
    |Character of character
    |Effect of (entity * int)
    |Event of event

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

(*************************** KERRI STUFF BELOW *****************************)

      (*TODO: use official ones*)
  type state = {
    event:event;
    characters: character list;
    output: string;
  }

let alter_state st ?(evt=st.event) ?(chars=st.characters) output =
  {
    event=evt;
    characters=chars;
    output=output;
  }

(** SHOP **)

let buy_item name evt = ()
    (*
  match List.find_opt (fun x -> x.name =name) (E.get_items evt) with
  | Some i -> failwith "unimplemented"
  | None -> failwith "unimplemented"*)

(** COMBAT **)

let attack a t evt st:state =
  let ac = List.find_opt (fun x -> C.name x = a) st.characters in
  let tc = List.find_opt (fun x -> C.name x = t) st.characters in
  try
    match E.attack_opt ac a tc t evt with
    | (evt', None) -> alter_state st ~evt:evt' (E.get_output evt)
    | (evt', Some c) ->
      let c' =
        List.map (fun x -> if C.name x = C.name c then c else x) st.characters
      in
      alter_state st ~evt:evt' ~chars:c' (E.get_output evt)
  with _ -> alter_state st "Action Failed: Invalid Character Name"

(*************************** KERRI STUFF ABOVE *****************************)

(*let action c (st:state) =
  match c with
  | Fight (a,b) -> attack a b st.event st
  | _ -> failwith "unimplemented" *)

end
