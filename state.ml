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

  let init_state d = d(*TODO: uuuJuUuh *)
  let current_location st = st.current_location.name
  let current_gamestate st = failwith "unimplemented"
  let current_room_characters st = st
  let rooms st = failwith "unimplemented"
  let effects st = failwith "unimplemented"
  let event st = st.event

  (*still needs to add remove_item and support giving more than one item*)
  let give st item p1 p2 ?(q=1) = failwith "unimplemented"
      (* )
    let c1 = (List.find (fun x -> fst x = p1)) in
    let c1' = ((*C.remove_item*) fst c1, snd c1) in
    let c2 = C.add_item (List.filter (fun x -> fst x = p2)) item in
    let c2' = (C.add_item (fst c2) item, snd c2) in
    let clist = (List.filter (fun x -> (fst x != c1) || (fst x != c2))) in
         {st with characters = c1'::c2'::clist} *)

(*************************** KERRI STUFF BELOW *****************************)

  let alter_state st ?(currLoc=st.current_location)
      ?(evt=st.event) ?(chars=st.characters) output =
  {
    current_location = currLoc;
    locations = st.locations;
    event=evt;
    characters=chars;
    output=output;
  }

let update_char c c' ?(r'=None) st =
  match r' with
  | None -> List.map (fun (x,r) -> if x=c then (c',r) else (x,r)) st.characters
  | Some r' ->
    List.map (fun (x,r) -> if x=c then (c',r') else (x,r)) st.characters

(** SHOP **)
let buy c i q evt st =
  let c' = C.add_item c i in (*TODO: add quantity, remove cost*)
  let evt' = E.remove_item i.name q evt in
  alter_state st ~evt:evt' ~chars:(update_char c c' st) "Items bought."

let buy_item c name q evt st=
  match List.find_opt (fun ((x:character),_) -> x.name = c) st.characters with
  | None -> alter_state st "Action Failed: Invalid character name."
  | Some (c,_) ->
    if Event.get_form evt <> Shop then
      alter_state st "Action Failed: There is no shop here."
    else
      match List.find_opt (fun ((x:item),_) -> x.name =name) (E.get_items evt) with
      | None -> alter_state st "Action Failed: That item is not available."
      | Some (i, Infinity) -> buy c i (Int q) evt st
      | Some (i, Int n) ->
        if n<q then
          alter_state st ("Action Failed: There are only "^(string_of_int n)^" available.")
        else
          buy c i (Int n) evt st


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
      let chars = update_char t t' st in
      alter_state st ~evt:evt' ~chars:chars
        ((C.name a)^" attacked "^(C.name t)^"!")

(*************************** KERRI STUFF ABOVE *****************************)

let action (c:command) (st:state) =
  match c with
  | Fight (a,b) -> attack a b st.event st
  | Buy (ch,i,q) -> begin
      try buy_item ch i (int_of_string q) st.event st
      with _ -> alter_state st "Invalid item quantity."
    end
  | _ -> failwith "unimplemented"

let output st = st.output

end
