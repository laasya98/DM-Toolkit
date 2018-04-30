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

  type role = PC | Hostile | Friendly | Neutral

  type data = D.data
  type character = Character.c
  type event = Event.t
  type command = Com.command

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

  type state = {
    locations : location list;
    characters : (character * role) list;
    event : event;
    output :string;
    current_location : location;
  }

  val init_state : D.data -> state
  val current_room : state -> string
  val current_gamestate : state -> string
  val current_room_characters : state -> string list
  val rooms : state -> string list
  val effects : state -> string list
  val event : state -> event
  val action : Com.command -> state -> state
  val output : state -> string
end

module State = struct
  module D = Database
  module C = Character
  module E = Event
  module Com = Command

  type character = Character.c
  type event = Event.t
  type command = Com.command
  type data = D.data

  type role = PC | Hostile | Friendly | Neutral

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

  type state = {
    locations : location list;
    characters : (character * role) list;
    event : event;
    output :string;
    current_location : location;
  }

  let init_state d = failwith "unimplemented"
  let current_room st = failwith "unimplemented"
  let current_gamestate st = failwith "unimplemented"
  let current_room_characters st = failwith "unimplemented"
  let rooms st = failwith "unimplemented"
  let effects st = failwith "unimplemented"
  let event st = st.event
  let give st item p1 p2 q = failwith "unimplemented: requires method to remove items from inventory in character"

(*************************** KERRI STUFF BELOW *****************************)

let alter_state st ?(evt=st.event) ?(chars=st.characters) output =
  {
    locations = st.locations;
    event=evt;
    characters=chars;
    output=output;
  }

(** SHOP **)

let buy_item c name q evt st=
  if Event.get_form evt <> Shop then
    alter_state st "Action Failed: There is no shop here."
  else
    match List.find_opt (fun ((x:item),_) -> x.name =name) (E.get_items evt) with
    | None -> alter_state st "Action Failed: That item is not available."
    | Some (i, Infinity) -> failwith "unim"
    | Some (i, Int n) ->
      if n<q then
        alter_state st ("There are only "^(string_of_int n)^" available.")
      else
        failwith "unim"


(** COMBAT **)

let attack a t evt st:state =
  let ac = List.find_opt (fun (x,_) -> C.name x = a) st.characters in
  let tc = List.find_opt (fun (x,_) -> C.name x = t) st.characters in
  match ac with
  | None -> alter_state st "Action Failed: Invalid Attacker Name"
  | Some (a,_) ->
    match tc with
    | None -> alter_state st "Action Failed: Invalid Target Name"
    | Some (t,_) -> let (evt', t') = E.attack a t evt in
      let chars = List.map (fun (x,r) -> if x=t then (t',r) else (x,r)) st.characters in
      alter_state st ~evt:evt' ~chars:chars
        ((C.name a)^" attacked "^(C.name t)^"!")

(*************************** KERRI STUFF ABOVE *****************************)

let action (c:command) (st:state) =
  match c with
  | Fight (a,b) -> attack a b st.event st
  | _ -> failwith "unimplemented"

let output st = st.output

end
