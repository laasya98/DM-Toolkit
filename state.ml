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

let empty_location = {
  name = "empty";
  description = "no description";
  characters = [];
  contents = [];
  exits = [];
}

let empty_state = {
  locations = [];
characters = [];
event = init_event "";
output = "";
current_location = empty_location;
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

let current_location st = alter_state st st.current_location.name
let current_room_characters st = failwith "unimplemented"
let rooms st = failwith "unimplemented"
let effects st = failwith "unimplemented"
let event st = st.event

(** [get_exits st] returns a list of strings that represents the exits for the
    current room.*)
let get_exits st =
  List.map (fun x -> fst x) (st.current_location.exits)


(* [move st dir] is the state with current_room now the room reflected by dir
   from the original current_room. It leaves all non-party characters in the
   previous room. It's output is "Party moves *dir*" with a description*)
let move_dir st dir =
  if not (List.mem dir (List.map (fun x -> fst x) st.current_location.exits))
  then alter_state st "Not a direction"
  else let newlocation =
         snd (List.find (fun x -> fst x = dir) st.current_location.exits) in
    let newcharacters =
      (List.filter (fun x -> snd x = Party) st.characters)
      @ newlocation.characters in
    alter_state st ~currLoc:newlocation ~chars:newcharacters
      ("Party moves" ^dir ^"\n\n\n" ^ newlocation.description)

(** [character_list_filter ls role] returns a string delimited by commas
  representing the characters in a list that match the given role *)
let character_list_filter ls role =
  ((List.filter (fun x -> snd x = role) ls)
   |> List.map (fun x -> C.name (fst x)))

let character_list_string ?role:(r = All) st =
  match r with
  |All -> (List.map (fun (x,y) -> C.name x) st.characters) |> String.concat ", "
  |role -> character_list_filter st.characters role |> String.concat ", "


(*still needs to add remove_item and support giving more than one item*)
let give st ?(q=1) item p1 p2 =
  try
    let c1 = (List.find (fun x -> fst x = p1) st.characters) in
    let c1' = (C.remove_item (fst c1) item q), snd c1 in
    let c2 = (List.find (fun x -> fst x = p2) st.characters) in
    let c2' = (C.add_item (fst c2) item q), snd c2 in
    let clist = (List.filter (fun x -> (fst x <> p1)) st.characters) in
    let clist' = (List.filter (fun x -> (fst x != p2)) clist) in
    alter_state {st with characters = c1'::c2'::clist'} (item.name ^ "given")
  with _ -> alter_state st ("One or more items or characters missing")

let update_char c c' ?(r'=None) st =
match r' with
| None -> List.map (fun (x,r) -> if x=c then (c',r) else (x,r)) st.characters
| Some r' ->
  List.map (fun (x,r) -> if x=c then (c',r') else (x,r)) st.characters

let update_chars cs st =
  let rec r cs lst =
  match cs with
  | [] -> lst
  | c::t -> r t (List.map
     (fun (x,r) -> if C.name x = C.name c then (c,r)
      else (x,r)) lst)
  in
  r cs st.characters

(** SHOP **)

let buy c i q evt st =
  let m = i.value * q in
  let cm = C.money c in
  if cm < m then alter_state st "Character doesn't have enough money."
  else
    let c' = C.update_money (C.add_item c i q) (cm-m) in
    let evt' = E.remove_item i.name (Int q) evt in
    alter_state st ~evt:evt' ~chars:(update_char c c' st) "Items bought."

let sell c i q evt st =
  let m = i.value * q / 10 in
  let cm = C.money c in
  let c' = C.update_money (C.remove_item c i q) (cm+m) in
  let evt' = E.add_item i (Int q) evt in
  alter_state st ~evt:evt' ~chars:(update_char c c' st) "Items sold."

let use c i q evt st =
  let (evt', t') = E.use_item i c evt in
  let chars = update_chars t' st in
  alter_state st ~evt:evt' ~chars:chars (E.verbose evt')

let item_helper c i q inv f evt st =
  match List.find_opt (fun ((x:character),_) -> x.name = c) st.characters with
  | None -> alter_state st "Action Failed: Invalid character name."
  | Some (c,_) ->
    let stock = if inv then C.inv c else E.get_items evt in
    match List.find_opt (fun ((x:item),_) -> x.name =i) stock with
    | None ->  alter_state st "Action Failed: That item is not available."
    | Some (i, Infinity) -> f c i q evt st
    | Some (i, Int n) ->
      if n<q then
        alter_state st ("Action Failed: There are only "^(string_of_int n)^" available.")
      else
        f c i q evt st

let b_s_item c i q evt st buying =
  match E.get_form st.event with
  | Shop -> begin
      try
        let q' = int_of_string q in
        if buying then
          item_helper c i q' false buy evt st
        else
          item_helper c i q' true sell evt st
    with _ -> alter_state st "Invalid item quantity."
  end
  |_ -> alter_state st "Action Failed: There is no shop here."

let use_item i c evt st= item_helper c i 1 true use evt st

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
      alter_state st ~evt:evt' ~chars:chars (E.verbose evt')

let char_by_name s st =
  let c = List.find_opt (fun (x,_) -> C.name x = s) st.characters in
  match c with
  | None -> None
  | Some (x,_) -> Some x

let cast c s t evt st =
  let c' = char_by_name c st in
  let s' = failwith "match s with a spell in char spells" in
  try
    let find s = List.find (fun (x,_) -> C.name x = s) st.characters in
    let t' = List.map (fun s -> fst (find s)) t in
    match c' with
    | None -> alter_state st "The castor name is invalid."
    | Some c ->
      let (evt', t') = E.cast c s' t' evt in
      let chars = update_chars t' st in
      alter_state st ~evt:evt' ~chars:chars (E.verbose evt')
  with _ -> alter_state st "One or more target names invalid."

let use_item i c evt st =
  let c' = char_by_name c st in
  match c' with
  | None -> alter_state st "Character name invalid."
  | Some c ->
    try
      let (i',_) = List.find (fun ((x:item ),n) -> x.name=i) (C.inv c) in
      let (evt', t') = E.use_item i' c evt in
      let chars = update_chars t' st in
      alter_state st ~evt:evt' ~chars:chars (E.verbose evt')
    with _ -> alter_state st "Item not found in character inventory."

let destribute_xp n st = failwith "unimplemented"

let remove_char c st =
  let cs = List.filter (fun ((x:character),_) -> x.name <> c) st.characters in
  alter_state st ~chars:cs "Character removed."

let kill_char c st =
  let c' = char_by_name c st in
  match c' with
  | None -> alter_state st "Character name invalid."
  | Some c ->
    let st' = remove_char c.name st in
    alter_state st' "Character killed."

let string_of_role r =
  match r with
  | All -> "All"
  | Party -> "Party"
  | Hostile -> "Hostile"
  | Friendly -> "Friendly"
  | Neutral -> "Neutral"

let print_char_short (c,r) =
  (C.name c)^"\t"^(string_of_role r)^"\tHP: "^(string_of_int (C.curr_hp c))

let print_chars_short st =
  List.map print_char_short st.characters
  |> List.fold_left (fun x a-> x^"\n"^a) ""

let gen_printout st =
  let evt = st.event in
  match E.get_form evt with
  |Battle ->
  (st.output)^
  "\n\nTurn number "^(string_of_int (E.get_turn evt))^".\n"
  ^(List.hd (E.get_turnlst evt))^"'s turn.\n\n"^(print_chars_short st)
  |_ -> st.output

let action (c:command) (st:state) =
  E.clear_vout st.event;
  match c with
  | Fight (a,b) -> begin
    match E.get_form st.event with
    | Battle -> attack a b st.event st
    | _ -> alter_state st "No battle event occurring."
  end
  | Cast (c,s,t) -> cast c s t st.event st
  | UseItem (c,i) -> item_helper c i 1 true use st.event st
  | Buy (ch,i,q) -> b_s_item ch i q st.event st true
  | Sell (ch,i,q) -> b_s_item ch i q st.event st false
  | Turn -> let (evt', t') = E.turn st.event in
    let chars = update_chars t' st in
    alter_state st ~evt:evt' ~chars:chars ("Turn incremented.\n"^(E.verbose evt'))
  | Move r -> (try move_dir st r with _-> alter_state st "No such direction.")

  | GetCharacterList r -> begin
      let lst = begin match r with
      |All -> character_list_string  st
      |role -> character_list_string ~role:role st
      end
    in alter_state st lst
  end
  |GetExits -> alter_state st (String.concat ", " (get_exits st))
  |Roll d -> alter_state st (string_of_int (Global.roll_dice_string d))
  |QuickBuild lst -> begin try
    let n = List.hd lst in
    let c = List.hd (List.tl lst) in
    let r = List.nth lst 2 in
    let newchar = C.quickbuild n c r in
    let newcharls = ((newchar,Party) :: st.characters) in
    alter_state st ~chars:newcharls ("New Character, " ^ n ^ ", added to party!")
      with _-> alter_state st "Failed. Your arguments might be off." end
  |QuickEvent (n,f) -> begin
      let f' = match f with
      |"battle" -> Battle
      |"shop" -> Shop
      | _ -> Interaction
      in
      let c = List.map fst st.characters in
      let evt = E.make_event n f' [] c in
      alter_state st ~evt:evt "Event started."
  end
  | _ -> alter_state st "Invalid move. Try again. Type \"help commands\" for a list of commands"

let output st = st.output
