open Database
open Event
open Character
open Command
open Global

  module D = Database
  module C = Character
  module E = Event
  module Com = Command

  type character = Character.c
  type event = Event.t
  type command = Com.command
  type data = D.data

  type role = Party | Hostile | Friendly | Neutral

  type entity =
    |Item of item
    |Effect of (entity * int)
    |Event of event

  type location = {
    name : string;
    description : string;
    characters : (character * role) list;
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

(** [alter_state] st currLoc evt chars output is a function for conveniently
    changing the fields in state and providing an output for main to display.*)
  let alter_state st ?(currLoc=st.current_location)
      ?(evt=st.event) ?(chars=st.characters) output =
    {
      current_location = currLoc;
      locations = st.locations;
      event=evt;
      characters=chars;
      output=output;
    }

  let init_state st = failwith "Unimplemented"(*TODO: uuuJuUuh *)

  let current_location st = st.current_location.name
  let current_room_characters st = failwith "unimplemented"
  let rooms st = failwith "unimplemented"
  let effects st = failwith "unimplemented"
  let event st = st.event

(*TODO: update list of locations to reflect   *)
  let move st dir =
    if not (List.mem dir (List.map (fun x -> fst x) st.current_location.exits))
    then alter_state st "Not a direction"
    else let newlocation =
           snd (List.find (fun x -> fst x = dir) st.current_location.exits) in
      let newcharacters =
        (List.filter (fun x -> snd x = Party) st.characters)
        @ newlocation.characters in
      alter_state st ~currLoc:newlocation ~chars:newcharacters
        ("Party moves" ^dir)

(** [character_list_filter ls role] returns a string delimited by commas
    representing the characters in a list that match the given role *)
  let character_list_filter ls role =
    String.concat (", ") @@
    ((List.filter (fun x -> snd x = role) ls)
     |> List.map (fun x -> fst x))


  (*still needs to add remove_item and support giving more than one item*)
  let give st item p1 p2 (* ?(q=1) *) = failwith "unimplemented"
      (* )
    let c1 = (List.find (fun x -> fst x = p1)) in
    let c1' = ((*C.remove_item*) fst c1, snd c1) in
    let c2 = C.add_item (List.filter (fun x -> fst x = p2)) item in
    let c2' = (C.add_item (fst c2) item, snd c2) in
    let clist = (List.filter (fun x -> (fst x != c1) || (fst x != c2))) in
         {st with characters = c1'::c2'::clist} *)

(*************************** KERRI STUFF BELOW *****************************)


let update_char c c' ?(r'=None) st =
  match r' with
  | None -> List.map (fun (x,r) -> if x=c then (c',r) else (x,r)) st.characters
  | Some r' ->
    List.map (fun (x,r) -> if x=c then (c',r') else (x,r)) st.characters

(** SHOP **)
let buy c i q evt st =
  let m = i.value * q in
  let cm = C.money c in
  if cm < m then alter_state st "Character doesn't have enough money."
  else
    let c' = C.update_money (C.add_item c i q) (cm-m) in
    let evt' = E.remove_item i.name (Int q) evt in
    alter_state st ~evt:evt' ~chars:(update_char c c' st) "Items bought."

let buy_item c name q evt st=
  match List.find_opt (fun ((x:character),_) -> x.name = c) st.characters with
  | None -> alter_state st "Action Failed: Invalid character name."
  | Some (c,_) ->
    match List.find_opt (fun ((x:item),_) -> x.name =name) (E.get_items evt) with
    | None -> alter_state st "Action Failed: That item is not available."
    | Some (i, Infinity) -> buy c i q evt st
    | Some (i, Int n) ->
      if n<q then
        alter_state st ("Action Failed: There are only "^(string_of_int n)^" available.")
      else
        buy c i q evt st


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
  | Fight (a,b) -> begin
    match E.get_form st.event with
    | Battle -> attack a b st.event st
    | _ -> alter_state st "No battle event occurring."
  end
  | Buy (ch,i,q) -> begin
      match E.get_form st.event with
      | Shop -> begin
        try buy_item ch i (int_of_string q) st.event st
        with _ -> alter_state st "Invalid item quantity."
      end
      |_ -> alter_state st "Action Failed: There is no shop here."
    end
  | _ -> failwith "unimplemented"

let output st = st.output
